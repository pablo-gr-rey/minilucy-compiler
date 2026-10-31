open Asttypes

type oc_var = Ident.t * base_ty

and oc_expr =
  | OCE_const of const
  | OCE_var of Ident.t (* local variable x *)
  | OCE_state of Ident.t (* state variable x *)
  | OCE_unop of unop * oc_expr
  | OCE_binop of binop * oc_expr * oc_expr

and oc_stmt =
  | OCS_assign of Ident.t * oc_expr (* assignment of a local variable x *)
  | OCS_state_assign of Ident.t * oc_expr (* assignment of a state variable x *)
  | OCS_reset of Ident.t (* o.reset() *)
  | OCS_step of Ident.t list * Ident.t * oc_expr list (* (x_1, ..., x_k) = o.step(e_1, ..., e_l) *)
  | OCS_prim of Ident.t list * Ident.t * oc_expr list (* (x_1, ..., x_k) = prim(e_1, ..., e_l) *)
  | OCS_case of Ident.t * oc_stmt list * oc_stmt list (* if (x) {s_1} else {s_2} *)

and oc_definition = {
  ocd_name : Ident.t; (* f *)
  ocd_mem : oc_var list; (* m *)
  ocd_insts : (Ident.t * Ident.t) list; (* j: list of (o: f) *)
  ocd_reset : oc_stmt list; (* reset() = S *)
  ocd_step : oc_var list * oc_var list * oc_var list * oc_stmt list;
      (* step(pat) returns (p) = var (p) in S *)
}

and oc_context = {
  occ_mem : oc_var list;
  occ_init : oc_stmt list;
  occ_insts : (Ident.t * Ident.t) list;
  occ_vars : oc_var list;
  occ_instrs : oc_stmt list;
}
