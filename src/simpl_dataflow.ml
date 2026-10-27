(* TODO: the normalization process creates a lot of useless temp variables, e.g. a = temp and temp = ... (or (temp1, temp2) = ...) that we can replace if they only appear once otherwise; we also should simplify variables with the same definition (if e1, e2 are local variables with the same definition, then we should merge them)
  This only works once we have completed clock inference; otherwise, if a: bool and b: bool when a, then we cannot replace b by a even if we have equations b = true and a = true *)

open Asttypes
open Typed_ast
open Norm_ast

type var_kind = V_input | V_output | V_local
type var_type = { v_var : typed_var; v_kind : var_kind }

let v_compare ((vi1, _, _) : typed_var) ((vi2, _, _) : typed_var) = compare vi1.id vi2.id

module VMap = Map.Make (struct
  type t = typed_var

  let compare = v_compare
end)

module EMap = Map.Make (struct
  type t = n_eq

  let compare = compare
end)

let simpl_dataflow node =
  (* goal: if we find an equation a = b, then we remove one and replace all uses of it by the other one; if we find two equations a = expr and b = expr (if at least one of them is local) then we merge a and b *)

  (* we build a map of variables to kind, and a map of definitions to variable list *)
  (* these two maps contain all information about the node, so we update them and later re-build the node from it *)
  let v2kind = ref VMap.empty and def2v = ref EMap.empty in

  let add_vkind kind v = v2kind := VMap.add v kind !v2kind in

  List.iter (add_vkind V_input) node.nn_input;
  List.iter (add_vkind V_local) node.nn_local;
  List.iter (add_vkind V_output) node.nn_output;

  (* var_rep contains tuple (old_var, new_var) which instructs to replace old_var by new_var *)
  let var_rep = ref [] in

  (* let add_repl v1 v2 =

  let add_eq map eq =
    (* if eq is already present, then we do not add the new equation and instead inform about a renaming *)
    if EMap.mem eq map then
    match eq with
    | NEQ_ce (x, _) | NEQ_fby (x, _, _) | NEQ_pre (x, _) -> EMap.add eq [ x ] map
    | NEQ_app (patt, _, _) | NEQ_prim (patt, _, _) -> EMap.add eq patt.tpatt_desc map
  in

  let def2v = List.fold_left add_eq def2v node.nn_equs in *)
  node
