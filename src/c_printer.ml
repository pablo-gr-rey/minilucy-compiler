open Asttypes
open Objc_ast
open Typed_ast_printer
open Norm
open Format
open C_utils

let get_ctype = function Tint -> "int" | Tbool -> "bool" | Treal -> "double"

(* we replace nil by an arbitrary value *)
let print_c_const fmt = function
  | Cnil ty -> print_const fmt (make_dummy_const ty)
  | c -> print_const fmt c

let rec print_unop ctx def fmt u oce =
  let s = match u with Unop_not -> "!" | Unop_neg | Unop_neg_f -> "-" in
  fprintf fmt "%s(@[%a@])" s (print_cexpr ctx def) oce

and print_binop ctx def fmt b oce1 oce2 =
  if b = Binop_impl then
    fprintf fmt "@[!(@[%a@])@ ||@ @[%a@]@]" (print_cexpr ctx def) oce1 (print_cexpr ctx def) oce2
  else
    let s =
      match b with
      | Binop_eq -> "=="
      | Binop_neq -> "!="
      | Binop_lt -> "<"
      | Binop_le -> "<="
      | Binop_gt -> ">"
      | Binop_ge -> ">="
      | Binop_add -> "+"
      | Binop_sub -> "-"
      | Binop_mul -> "*"
      | Binop_div -> "/"
      | Binop_mod -> "%"
      | Binop_add_f -> "+"
      | Binop_sub_f -> "-"
      | Binop_mul_f -> "*"
      | Binop_div_f -> "/"
      | Binop_and -> "&&"
      | Binop_or -> "||"
      | Binop_impl -> assert false
    in
    fprintf fmt "@[(@[%a@])@ %s@ (@[%a@])@]" (print_cexpr ctx def) oce1 s (print_cexpr ctx def) oce2

and print_cexpr ctx def fmt = function
  | OCE_const c -> print_c_const fmt c
  | OCE_var var -> begin
      if not (VMap.mem var def.var_map) then
        error dummy_loc "unknown var %a (not in var_map)" Ident.print_full var;
      match VMap.find var def.var_map with
      | V_input | V_local -> fprintf fmt "%a" Ident.print_full var
      | V_output -> fprintf fmt "%a.%a" Ident.print_full def.ret_name Ident.print_full var
    end
  | OCE_state var -> fprintf fmt "self->%a" Ident.print_full var
  | OCE_unop (u, oce) -> (print_unop ctx def) fmt u oce
  | OCE_binop (b, oce1, oce2) -> (print_binop ctx def) fmt b oce1 oce2

(* ctx maps each instance used in the current node to a couple with the corresponding node_info and the name of the local variable used to store the result of the call *)
and print_cstmt ctx def fmt stmt =
  (* if var is an output, then print result.var, otherwise var *)
  let print_var fmt var =
    if not (VMap.mem var def.var_map) then
      error dummy_loc "unknown var %a (not in var_map)" Ident.print_full var;
    match VMap.find var def.var_map with
    | V_input | V_local -> fprintf fmt "%a" Ident.print_full var
    | V_output -> fprintf fmt "%a.%a" Ident.print_full def.ret_name Ident.print_full var
  in

  match stmt with
  | OCS_assign (var, oce) ->
      fprintf fmt "@[%a = @[%a@];@]" print_var var (print_cexpr ctx def) oce
      (* begin
      if not (VMap.mem var def.var_map) then
        error dummy_loc "unknown var %a (not in var_map)" Ident.print_full var;
      match VMap.find var def.var_map with
      | V_input | V_local ->
          fprintf fmt "@[%a = @[%a@];@]" Ident.print_full var (print_cexpr ctx def) oce
      | V_output ->
          fprintf fmt "@[%a.%a = @[%a@];@]" Ident.print_full def.ret_name Ident.print_full var
            (print_cexpr ctx def) oce
    end *)
  | OCS_state_assign (var, oce) ->
      fprintf fmt "@[self->%a = @[%a@];@]" Ident.print_full var (print_cexpr ctx def) oce
  | OCS_reset obj ->
      let node = fst (VMap.find obj ctx) in
      (* if the instance is memoryless, it has no reset method, so we just drop the statement *)
      if Option.is_some node.node_reset then
        fprintf fmt "@[%a(&self->%a);@]" Ident.print_full (Option.get node.node_reset)
          Ident.print_full obj
  | OCS_step (vars, obj, args) ->
      let called_node, ret_var = VMap.find obj ctx in
      let assign x (field, _) =
        fprintf fmt "@[%a = %a.%a;@]" print_var x Ident.print_full ret_var Ident.print_full field
        (* print_assign fmt Ident.print_full field x *)
        (* fprintf fmt "@[%a = %a.%a;@]@;" Ident.print_full x Ident.print_full ret_var Ident.print_full
          field *)
      in
      let rec iter_assign vars fields =
        match (vars, fields) with
        | [], [] -> ()
        | [ v ], [ f ] -> assign v f
        | v :: vs, f :: fs ->
            assign v f;
            fprintf fmt "@;";
            iter_assign vs fs
        | _ -> error dummy_loc "invalid iter_assign call (different lengths)"
      in
      fprintf fmt "@[%a = %a(@[%a" Ident.print_full ret_var Ident.print_full called_node.node_step
        (print_list (print_cexpr ctx def) ", ")
        args;
      (* if the node also has memory, then we must also pass the corresponding instance *)
      (* (and add a comma if there already were arguments before) *)
      if Option.is_some called_node.node_mem then
        fprintf fmt "%s&self->%a" (if args <> [] then ", " else "") Ident.print_full obj;
      fprintf fmt "@]);@]";
      (* List.iter2 assign vars called_node.node_ret.struct_fields *)
      iter_assign vars called_node.node_ret.struct_fields
      (* return struct should have no instances, only fields *)
  | OCS_prim (vars, func, args) ->
      begin match (vars, func.name, args) with
      | [ x ], "int_of_real", [ oce ] ->
          fprintf fmt "@[%a = (int)(@[%a@]);@]" print_var x (print_cexpr ctx def) oce
      | [ x ], "real_of_int", [ oce ] ->
          fprintf fmt "@[%a = (double)(@[%a@]);@]" print_var x (print_cexpr ctx def) oce
      | _ -> error dummy_loc "invalid primitive call %s" func.name
      end
  | OCS_case (x, ocsT, ocsF) ->
      fprintf fmt "if (%a) {@;%a}@;else {@;%a}" print_var x (print_cstmts ctx true def) ocsT
        (print_cstmts ctx true def) ocsF

and print_cstmts ctx indent def fmt cstmts =
  if indent then fprintf fmt "@[<v 4>    " else fprintf fmt "@[<v>";
  print_list_eol (print_cstmt ctx def) "" fmt cstmts;
  (* List.iter (fun s -> fprintf fmt "%a@;" (print_cstmt ctx def) s) cstmts; *)
  fprintf fmt "@]@;"

let print_struct fmt str =
  let handle_field (id, ty) = (get_ctype ty, Ident.get_full id) in
  let handle_inst (id, str) = (Ident.get_full str.struct_name, Ident.get_full id) in
  let print_dbl_str fmt (s1, s2) = fprintf fmt "%s %s;" s1 s2 in
  let full_fields =
    List.map handle_field str.struct_fields @ List.map handle_inst str.struct_insts
  in
  fprintf fmt "typedef struct {@;@[<v 4>    %a@]} %a;@;" (print_list_eol print_dbl_str "")
    full_fields Ident.print_full str.struct_name

let print_node_res_signature fmt def =
  fprintf fmt "void %a(%a* self)" Ident.print_full (Option.get def.node_reset) Ident.print_full
    (Option.get def.node_mem).struct_name

let print_node_step_signature fmt def =
  let step_args, _, _, _ = def.node_origin.ocd_step in
  let print_arg fmt (x, ty) = Format.fprintf fmt "%s %a" (get_ctype ty) Ident.print_full x in
  let print_args fmt () =
    print_list print_arg ", " fmt step_args;
    if Option.is_some def.node_mem then
      let opt_comma = if step_args <> [] then ", " else "" in
      Format.fprintf fmt "%s%a* self" opt_comma Ident.print_full
        (Option.get def.node_mem).struct_name
  in
  Format.fprintf fmt "%a %a(%a)" Ident.print_full def.node_ret.struct_name Ident.print_full
    def.node_step print_args ()

let print_node defs_map fmt def =
  (* build context: we must map each instance (<=> each function call) to the couple of the corresponding node_info and the return variable *)
  let handle_inst (ctx, add_locals) ((obj, f) : Ident.t * Ident.t) =
    let ret_var = Ident.make (f.name ^ "_result") Ident.Object in
    if not (VMap.mem f defs_map) then
      error dummy_loc "unknown node %a for instance %a" Ident.print f Ident.print obj;
    let n_def = VMap.find f defs_map in
    (VMap.add obj (n_def, ret_var) ctx, (ret_var, n_def.node_ret) :: add_locals)
  in

  let ctx, add_locals = List.fold_left handle_inst (VMap.empty, []) def.node_origin.ocd_insts in
  (* we also need to add the result of the node to the new local variables *)
  let add_locals = (def.ret_name, def.node_ret) :: add_locals in

  let has_mem = Option.is_some def.node_reset in

  (* reset function: no argument, necessary only if node has memory *)
  if has_mem then
    Format.fprintf fmt "%a {@;%a}@;@;" print_node_res_signature def (print_cstmts ctx true def)
      def.node_origin.ocd_reset;

  (* step function: has arguments corresponding to the step function, plus a self parameter if the node has memory *)
  (* we add a new local variable corresponding to the return structure of the node, and return it at the end *)
  let _, _, step_loc, step_stmts = def.node_origin.ocd_step in
  let print_arg fmt (x, ty) = Format.fprintf fmt "%s %a" (get_ctype ty) Ident.print_full x in
  let print_add_loc fmt (var, str) =
    Format.fprintf fmt "%a %a" Ident.print_full str.struct_name Ident.print_full var
  in

  Format.fprintf fmt "%a {@;@[<v 4>    %a%a@;%a@;return %a;@]@;}@;" print_node_step_signature def
    (print_list_eol_last print_arg ";")
    step_loc
    (print_list_eol_last print_add_loc ";")
    add_locals (print_cstmts ctx false def) step_stmts Ident.print_full def.ret_name

let build_h_file basename defs_list =
  (* in the h file, we just print all structs *)
  let handle_node fmt node =
    fprintf fmt "// ------------------ NODE %s ------------------@;@;"
      node.node_origin.ocd_name.name;
    if Option.is_some node.node_mem then
      fprintf fmt "%a@;%a;@;@;" print_struct (Option.get node.node_mem) print_node_res_signature
        node;
    fprintf fmt "%a@;%a;@;@;" print_struct node.node_ret print_node_step_signature node
  in
  Format.asprintf
    "@[<v>// file auto-generated from Lustre file %s.lus@;#include <stdbool.h>@;@;%a@]" basename
    (print_list handle_node "") defs_list

let build_c_file filename defs_list defs_map =
  Format.asprintf "@[<v>// file auto-generated from Lustre file %s.lus@;#include \"%s.h\"@;@;%a@;@]"
    filename filename
    (print_list (print_node defs_map) "")
    defs_list

let build_main_file basename defs_list main_node_opt cmp_hept =
  let n_steps = 10000 in
  (* TODO: hardcoded *)
  let nodes =
    match main_node_opt with
    | Some main_node ->
        begin match
          List.find_opt (fun def -> def.node_origin.ocd_name.name = main_node) defs_list
        with
        | None -> error dummy_loc "No node named %s in the lus source" main_node
        | Some d -> [ d ]
        end
    | None -> defs_list
  in

  let print_includes fmt () =
    fprintf fmt "#include <stdio.h>@;";
    if cmp_hept then fprintf fmt "#include <stdlib.h>@;#include <math.h>@;";
    fprintf fmt "#include \"%s.h\"@;" basename;
    if cmp_hept then
      fprintf fmt "#include \"hept/%s\"@;"
        (Hept_utils.get_hept_node_info basename (List.hd nodes).node_origin.ocd_name.name)
          .header_name
  in

  let print_ctyfmt_in fmt (_, ty) =
    fprintf fmt "%s" (match ty with Tbool -> "%c" | Tint -> "%d" | Treal -> "%lf")
  in
  let print_ctyfmts_in = print_list print_ctyfmt_in " " in
  let print_ctyfmt_out fmt (_, ty) =
    fprintf fmt "%s" (match ty with Tbool | Tint -> "%d" | Treal -> "%lf")
  in
  let print_ctyfmts_out = print_list print_ctyfmt_out ", " in

  let print_node_test fmt node =
    let input, ret, _, _ = node.node_origin.ocd_step in
    let hept_info = Hept_utils.get_hept_node_info basename node.node_origin.ocd_name.name in
    let has_mem = Option.is_some node.node_mem in
    let nn_name = node.node_origin.ocd_name in

    let print_node_mem fmt () =
      (* normally, hept has the same criteria as us to generate a memory struct *)
      match (node.node_mem, node.node_reset) with
      | Some str, Some reset_name ->
          fprintf fmt "%a mem_%a;@;%a(&mem_%a);@;" Ident.print_full str.struct_name Ident.print_full
            nn_name Ident.print_full reset_name Ident.print_full nn_name;
          if cmp_hept then
            fprintf fmt "%s hept_mem_%a;@;%s(&hept_mem_%a);@;" hept_info.mem_type Ident.print_full
              nn_name hept_info.reset_fun Ident.print_full nn_name
      | _ -> ()
    in

    let print_ret_input fmt () =
      let dec_input fmt (id, ty) =
        (* if we're using stdio, for bool, we need to be able to read 1/T/t or 0/F/f, so we need to read a temp char and then convert to bool *)
        match (ty, cmp_hept) with
        | Tbool, false ->
            fprintf fmt "%s input_%a_%a;@;char inputtmp_%a_%a;@;" (get_ctype ty) Ident.print_full
              nn_name Ident.print_full id Ident.print_full nn_name Ident.print_full id
        | _ ->
            fprintf fmt "%s input_%a_%a;@;" (get_ctype ty) Ident.print_full nn_name Ident.print_full
              id
      in
      let print_hept_ret fmt () =
        if cmp_hept then fprintf fmt "%s hept_ret_%a;@;" hept_info.out_type Ident.print_full nn_name
      in
      fprintf fmt "%a ret_%a;@;%a%a@;" Ident.print_full node.node_ret.struct_name Ident.print_full
        nn_name print_hept_ret () (print_list dec_input "") input
    in

    let print_node_info fmt () =
      let dec_var fmt ((id, ty) : oc_var) = fprintf fmt "\\\"%s\\\":%s" id.name (get_ctype ty) in

      if cmp_hept then
        fprintf fmt "printf(\"\\ntesting node %a (file %s.lus) on %d steps...\\n\");@;"
          Ident.print_full node.node_origin.ocd_name basename n_steps
      else
        fprintf fmt "printf(\"#inputs %a\\n#outputs %a\\n\\n\");@;" (print_list dec_var ", ") input
          (print_list dec_var ", ") ret
    in

    let print_for_loop fmt () =
      let print_n_steps fmt () = if cmp_hept then fprintf fmt "step < %d" n_steps in
      fprintf fmt "for (int step = 0; %a; step++)" print_n_steps ()
    in

    let print_io_step fmt () =
      let scan_input fmt () =
        let bool_inputs = List.filter (fun (_, ty) -> ty = Tbool) input in
        let dec_input fmt (id, ty) =
          fprintf fmt "&input%s_%a_%a"
            (if ty = Tbool then "tmp" else "")
            Ident.print_full nn_name Ident.print_full id
        in
        let print_fail fmt () =
          fprintf fmt "{@;@[<v 4>    printf(\"Failed to read input; exiting\\n\");@;return 1;@]@;}"
        in
        let handle_bool fmt (id, _) =
          let print_tmp fmt () =
            fprintf fmt "inputtmp_%a_%a" Ident.print_full nn_name Ident.print_full id
          in
          let print_res fmt () =
            fprintf fmt "input_%a_%a" Ident.print_full nn_name Ident.print_full id
          in
          fprintf fmt
            "if (%a == '1' || %a == 'T' || %a == 't')@;\
            \    %a = true;@;\
             else {@;\
             @[<v 4>    %a = false;@;\
             if (%a != '0' && %a != 'F' && %a != 'f')@;\
            \    bool_read_OK = false;@]@;\
             }@;\
             if (!bool_read_OK) %a@;"
            print_tmp () print_tmp () print_tmp () print_res () print_res () print_tmp () print_tmp
            () print_tmp () print_fail ()
        in
        let handle_bools fmt () =
          if bool_inputs <> [] then begin
            fprintf fmt "bool bool_read_OK = true;@;%a" (print_list handle_bool "") bool_inputs
          end
        in
        if input <> [] then
          fprintf fmt "if (scanf(\" %a\", %a) != %d) %a@;@;%a@;" print_ctyfmts_in input
            (print_list dec_input ", ") input (List.length input) print_fail () handle_bools ()
      in
      let apply_step fmt () =
        let dec_input fmt (id, _) =
          fprintf fmt "input_%a_%a" Ident.print_full nn_name Ident.print_full id
        in
        let dec_mem fmt () = fprintf fmt "&mem_%a" Ident.print_full nn_name in
        (* fprintf fmt "ret = %a(%a%s%s);@;@;" Ident.print_full node.node_step
        (print_list dec_input ", ") input
        (if has_mem && input <> [] then ", " else "")
        (if has_mem then "&mem" else "") *)
        fprintf fmt "ret_%a = %a(%a);@;@;" Ident.print_full nn_name Ident.print_full node.node_step
          (print_2_lists dec_input dec_mem ", ")
          (input, if has_mem then [ () ] else [])
      in
      let print_output fmt () =
        let dec_input fmt (id, _) =
          fprintf fmt "input_%a_%a" Ident.print_full nn_name Ident.print_full id
        in
        let dec_output fmt (id, _) =
          fprintf fmt "ret_%a.%a" Ident.print_full nn_name Ident.print_full id
        in
        fprintf fmt "printf(\"%a #outs %a\\n\\n\", %a);" print_ctyfmts_out input print_ctyfmts_out
          ret
          (* (print_list dec_input ", ") input
        (if input <> [] then ", " else "")
        (print_list dec_output ", ") ret *)
          (print_2_lists dec_input dec_output ", ")
          (input, ret)
      in
      fprintf fmt "printf(\"#step %%d\\n\", step);@;@;%a%a%a" scan_input () apply_step ()
        print_output ()
    in

    let print_hept_step fmt () =
      let write_input fmt (id, ty) =
        let print_rand fmt () =
          match ty with
          | Tbool -> fprintf fmt "(bool)(rand() & 1)"
          | Tint ->
              fprintf fmt "rand() %% 100 + 1"
              (* some nodes cannot handle inputs being 0 (ex. counter_modulo), of course this gives no guarantee but it seems to work well enough to test on most files *)
          | Treal -> fprintf fmt "(double)rand() / (double)RAND_MAX * 10.0"
        in
        fprintf fmt "input_%a_%a = %a;@;" Ident.print_full nn_name Ident.print_full id print_rand ()
      in
      let compute_res fmt () =
        let dec_input fmt (id, _) =
          fprintf fmt "input_%a_%a" Ident.print_full nn_name Ident.print_full id
        in
        let dec_mem fmt () = fprintf fmt "&mem_%a" Ident.print_full nn_name in
        let dec_hept_out fmt () = fprintf fmt "&hept_ret_%a" Ident.print_full nn_name in
        let dec_hept_mem fmt () = fprintf fmt "&hept_mem_%a" Ident.print_full nn_name in
        fprintf fmt "ret_%a = %a(%a);@;%s(%a);@;" Ident.print_full nn_name Ident.print_full
          node.node_step
          (print_2_lists dec_input dec_mem ", ")
          (input, if has_mem then [ () ] else [])
          hept_info.step_fun
          (print_3_lists dec_input dec_hept_out dec_hept_mem ", ")
          (input, [ () ], if has_mem then [ () ] else [])
      in
      let handle_output fmt () =
        let cmp_var fmt ((id, ty) : oc_var) =
          match ty with
          | Tint | Tbool ->
              fprintf fmt "hept_ret_%a.%s != ret_%a.%a" Ident.print_full nn_name id.name
                Ident.print_full nn_name Ident.print_full id
          | Treal ->
              fprintf fmt "fabs(hept_ret_%a.%s - ret_%a.%a) > 1e-3" Ident.print_full nn_name id.name
                Ident.print_full nn_name Ident.print_full id (* TODO: hardcoded *)
        in
        let dec_output fmt (id, ty) =
          fprintf fmt
            "if (%a) {@;\
             @[<v 4>    printf(\"ERROR at step %%d for node %a, field %a: got %a, expected \
             %a\\n\", step, ret_%a.%a, hept_ret_%a.%s);@;\
             return 1;@]@;\
             }@;"
            cmp_var (id, ty) Ident.print_full node.node_origin.ocd_name Ident.print_full id
            print_ctyfmt_out (id, ty) print_ctyfmt_out (id, ty) Ident.print_full nn_name
            Ident.print_full id Ident.print_full nn_name id.name
        in
        fprintf fmt "%a" (print_list dec_output "") ret
      in
      fprintf fmt "%a%a%a" (print_list write_input "") input compute_res () handle_output ()
    in

    let print_step fmt () = if cmp_hept then print_hept_step fmt () else print_io_step fmt () in

    let print_conclusion fmt () =
      if cmp_hept then
        fprintf fmt "@;printf(\"Ran for %d steps, got the same output!\\n\");" n_steps
    in

    fprintf fmt
      "@[<v 4>    // --------------- %s NODE %a FROM FILE %s.lus ---------------@;\
       @;\
       %a%a%a@;\
       %a {@;\
       @[<v 4>    %a@]@;\
       }%a@]@;"
      (if cmp_hept then "TESTING" else "SIMULATING")
      Ident.print_full node.node_origin.ocd_name basename print_node_mem () print_ret_input ()
      print_node_info () print_for_loop () print_step () print_conclusion ()
  in

  Format.asprintf
    "@[<v>// file auto-generated from Lustre file %s.lus@;%a@;int main(void) {@;%a}@;@]" basename
    print_includes ()
    (print_list_eol print_node_test "")
    nodes

let print_c_files defs basename gen_main gen_hept node =
  (* build the node infos *)
  let defs_map, defs_list = List.fold_left build_def_info (VMap.empty, []) defs in
  let defs_list = List.rev defs_list in
  (* we want to print first the earliest nodes *)

  let h_file = build_h_file basename defs_list in
  let c_file = build_c_file basename defs_list defs_map in
  let main_file =
    match (gen_main, gen_hept) with
    | true, _ -> Some (build_main_file basename defs_list node gen_hept)
    | false, true -> error dummy_loc "Cannot specify -compare-hept without -gen-main"
    | false, false -> None
  in

  (h_file, c_file, main_file)
