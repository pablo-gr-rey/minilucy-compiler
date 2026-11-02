open Asttypes
open Objc_ast
open Typed_ast_printer
open Objc_ast_printer
open Norm
open Format
open C_utils

let get_ctype = function Tint -> "int" | Tbool -> "bool" | Treal -> "float"

(* we replace nil by an arbitrary value *)
let print_c_const fmt = function
  | Cnil ty -> print_const fmt (make_dummy_const ty)
  | c -> print_const fmt c

let rec print_unop ctx def fmt u oce =
  let s = match u with Unop_not -> "!" | Unop_neg -> "-" in
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
          fprintf fmt "@[%a = (int)(@[%a@]);@]" Ident.print_full x print_ocexpr oce
      | [ x ], "real_of_int", [ oce ] ->
          fprintf fmt "@[%a = (float)(@[%a@]);@]" Ident.print_full x print_ocexpr oce
      | _ -> error dummy_loc "invalid primitive call %s" func.name
      end
  | OCS_case (x, ocsT, ocsF) ->
      fprintf fmt "if (%a) {@;%a}@;else {@;%a}" Ident.print_full x (print_cstmts ctx true def) ocsT
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
    Format.fprintf fmt "void %a(%a* self) {@;%a}@;@;" Ident.print_full (Option.get def.node_reset)
      Ident.print_full (Option.get def.node_mem).struct_name (print_cstmts ctx true def)
      def.node_origin.ocd_reset;

  (* step function: has arguments corresponding to the step function, plus a self parameter if the node has memory *)
  (* we add a new local variable corresponding to the return structure of the node, and return it at the end *)
  let step_args, _, step_loc, step_stmts = def.node_origin.ocd_step in
  let print_arg fmt (x, ty) = Format.fprintf fmt "%s %a" (get_ctype ty) Ident.print_full x in
  let print_args fmt () =
    print_list print_arg ", " fmt step_args;
    if has_mem then
      let opt_comma = if step_args <> [] then ", " else "" in
      Format.fprintf fmt "%s%a* self" opt_comma Ident.print_full
        (Option.get def.node_mem).struct_name
  in
  let print_add_loc fmt (var, str) =
    Format.fprintf fmt "%a %a" Ident.print_full str.struct_name Ident.print_full var
  in

  Format.fprintf fmt "%a %a(%a) {@;@[<v 4>    %a%a@;%a@;return %a;@]@;}@;" Ident.print_full
    def.node_ret.struct_name Ident.print_full def.node_step print_args ()
    (print_list_eol_last print_arg ";")
    step_loc
    (print_list_eol_last print_add_loc ";")
    add_locals (print_cstmts ctx false def) step_stmts Ident.print_full def.ret_name

let build_h_file defs_list =
  (* in the h file, we just print all structs *)
  let handle_node fmt node =
    if Option.is_some node.node_mem then
      fprintf fmt "%a@;%a" print_struct (Option.get node.node_mem) print_struct node.node_ret
    else fprintf fmt "%a" print_struct node.node_ret
  in
  Format.asprintf "@[<v>#include <stdbool.h>@;@;%a@]" (print_list handle_node "") defs_list

let build_c_file filename defs_list defs_map =
  Format.asprintf "@[<v>#include \"%s.h\"@;@;%a@;@]" filename
    (print_list (print_node defs_map) "")
    defs_list

let print_c_files defs filename =
  (* build the node infos *)
  let defs_map, defs_list = List.fold_left build_def_info (VMap.empty, []) defs in
  let defs_list = List.rev defs_list in
  (* we want to print first the earliest nodes *)

  let h_file = build_h_file defs_list in
  let c_file = build_c_file filename defs_list defs_map in

  (h_file, c_file)
