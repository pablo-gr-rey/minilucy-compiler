(* TODO: the normalization process creates a lot of useless temp variables, e.g. a = temp and temp = ... (or (temp1, temp2) = ...) that we can replace if they only appear once otherwise; we also should simplify variables with the same definition (if e1, e2 are local variables with the same definition, then we should merge them)
  This only works once we have completed clock inference; otherwise, if a: bool and b: bool when a, then we cannot replace b by a even if we have equations b = true and a = true *)

open Asttypes
open Typed_ast
open Norm_ast
open Typed_ast_printer

exception Error of location * string

let error loc fmt = Format.kasprintf (fun s -> raise (Error (loc, s))) fmt
let report fmt s = Format.fprintf fmt "%s" s

type var_kind = V_input | V_output | V_local
type var_type = { v_var : typed_var; v_kind : var_kind }

(* let v_compare ((vi1, _, _) : typed_var) ((vi2, _, _) : typed_var) = compare vi1.id vi2.id *)

module VMap = Map.Make (struct
  type t = Ident.t

  let compare = compare
end)

module EMap = Map.Make (struct
  type t = n_eq

  let compare = compare
end)

let simpl_dataflow node =
  (* goal: if we find an equation a = b, then we remove one and replace all uses of it by the other one *)
  (* TODO: if a = eq and b = eq, replace a by b? *)

  (* we build a map of variables to kind and typed_var to be able to choose between variables *)
  (* note that this replaces the node fields, which are later re-built from this one *)
  let error_ fmt = error node.nn_loc fmt in
  let v2kind = ref VMap.empty in

  let add_vkind kind (t : typed_var) =
    let id, _, _ = t in
    (* Format.printf "added variable %a@\n" Ident.print id; *)
    v2kind := VMap.add id (kind, t) !v2kind
  in
  List.iter (add_vkind V_input) node.nn_input;
  List.iter (add_vkind V_local) node.nn_local;
  List.iter (add_vkind V_output) node.nn_output;

  (* var_rep contains tuple (old_var, new_var) which instructs to replace old_var by new_var *)
  let var_rep = ref VMap.empty in
  (* final maps variable v to either Some f if it has been computed that the root of v is f, or None is the computation is ongoing to be able to detect cycles (at the end, it should not contain any None) *)
  let final = ref VMap.empty in

  (* if we find v1 = v2, replace one by the other *)
  let add_repl v1 v2 =
    if not (VMap.mem v1 !v2kind) then error_ "unknown variable %a" Ident.print v1;
    if not (VMap.mem v2 !v2kind) then error_ "unknown variable %a" Ident.print v2;
    let kind1, (_, ty1, ck1) = VMap.find v1 !v2kind
    and kind2, (_, ty2, ck2) = VMap.find v2 !v2kind in
    if ty1 <> ty2 then
      error_ "simpl error: found an equation %a = %a but type mismatch: %a vs %a" Ident.print v1
        Ident.print v2 print_base_type ty1 print_base_type ty2;
    if ck1 <> ck2 then
      error_ "simpl error: found an equation %a = %a but clock mismatch: %a vs %a" Ident.print v1
        Ident.print v2 print_clock ck1 print_clock ck2;

    let add vold vnew =
      if VMap.mem vold !var_rep then
        error_ "simpl error: variable %a replaced twice" Ident.print vold;
      var_rep := VMap.add vold vnew !var_rep
    in
    match (kind1, kind2) with
    (* if one is local, then replace this one; otherwise, do nothing *)
    | V_local, _ -> add v1 v2
    | _, V_local -> add v2 v1
    | _ -> ()
  in

  let build_final_repl () =
    (* var_rep should define a forest, we want to replace every variable by the corresponding root *)
    (* if there is a cycle, it means there is a causality problem *)
    final := VMap.empty;
    let rec compute v =
      match VMap.find_opt v !final with
      | None ->
          (* if v is not in var_rep, it means it is a root (should be replaced by itself) *)
          begin match VMap.find_opt v !var_rep with
          | None ->
              final := VMap.add v (Some v) !final;
              v
          | Some parent ->
              final := VMap.add v None !final;
              let true_parent = compute parent in
              final := VMap.update v (fun _ -> Some (Some true_parent)) !final;
              true_parent
          end
      | Some None ->
          error_ "causality problem: cyclic definition starting from var %a" Ident.print v
      | Some (Some true_parent) -> true_parent
    in
    VMap.iter (fun v _ -> ignore (compute v)) !var_rep
  in

  let find_repl v =
    match VMap.find_opt v !final with
    | None -> v
    | Some None -> error_ "none left in final after cycle detection"
    | Some (Some p) -> p
  in
  let repl_patt p = { p with tpatt_desc = List.map find_repl p.tpatt_desc } in

  let handle_eqs eq_list = function
    (* if eq is v1 = v2, then add the replacement, otherwise do nothing *)
    | NEQ_ce (v1, { ncexpr_desc = NCE_expr { nexpr_desc = NE_ident v2; _ }; _ }) ->
        add_repl v1 v2;
        eq_list
    | eq -> eq :: eq_list
  in

  let rec replace_ned = function
    | NE_const c -> NE_const c
    | NE_ident v -> NE_ident (find_repl v)
    | NE_unop (u, e) -> NE_unop (u, replace_ne e)
    | NE_binop (b, e1, e2) -> NE_binop (b, replace_ne e1, replace_ne e2)
    | NE_when (e, b, v) -> NE_when (replace_ne e, b, find_repl v)
  and replace_ne ne = { ne with nexpr_desc = replace_ned ne.nexpr_desc }
  and replace_nced = function
    | NCE_expr ne -> NCE_expr (replace_ne ne)
    | NCE_merge (v, nce1, nce2) -> NCE_merge (find_repl v, replace_nce nce1, replace_nce nce2)
  and replace_nce nce = { nce with ncexpr_desc = replace_nced nce.ncexpr_desc } in

  let replace_eq = function
    | NEQ_ce (v, nce) -> NEQ_ce (find_repl v, replace_nce nce)
    | NEQ_fby (v, c, ne) -> NEQ_fby (find_repl v, c, replace_ne ne)
    | NEQ_app (patt, f, args) -> NEQ_app (repl_patt patt, f, List.map replace_ne args)
    | NEQ_prim (patt, f, args) -> NEQ_prim (repl_patt patt, f, List.map replace_ne args)
  in

  let filter_var _ (v_kind, var) l = if v_kind = V_local then var :: l else l in

  let rec step eq_list =
    let trimmed_eqs = List.fold_left handle_eqs [] eq_list in

    if !var_rep = VMap.empty then eq_list
    else begin
      build_final_repl ();
      let n_eq_list = List.map replace_eq trimmed_eqs in
      (* this should only drop local variables! *)
      v2kind := VMap.filter (fun id _ -> not (VMap.mem id !var_rep)) !v2kind;
      (* Format.printf "finished one step!@\n";
      VMap.iter
        (fun vold vnew ->
          Format.printf "\treplaced %a by %a@\n" Ident.print vold Ident.print (Option.get vnew))
        !final;
      Format.printf "%a@\n" Norm_ast_printer.print_node
        { node with nn_local = VMap.fold filter_var !v2kind []; nn_equs = n_eq_list }; *)
      var_rep := VMap.empty;
      final := VMap.empty;
      step n_eq_list
    end
  in

  let final_eq_list = step node.nn_equs in

  { node with nn_local = VMap.fold filter_var !v2kind []; nn_equs = final_eq_list }
