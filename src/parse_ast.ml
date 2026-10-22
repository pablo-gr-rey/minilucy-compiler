(* Arbres de syntaxe abstraite *)

open Asttypes

type ident = string

type p_expr = { pexpr_desc : p_expr_desc; pexpr_loc : location }
and p_clock = PC_base | PC_when of ident | PC_whennot of ident

and p_expr_desc =
  | PE_const of const
  | PE_ident of ident
  | PE_op of op * p_expr list
  | PE_app of ident * p_expr list
  | PE_arrow of p_expr * p_expr
  | PE_pre of p_expr
  | PE_tuple of p_expr list
  | PE_when of
      p_expr * bool * ident (* true = e when x ; false = e when not x *)
  | PE_merge of
      ident
      * bool
      * p_expr
      * bool
      * p_expr (* merge x (b1 -> e, ..., e) (b2 -> e, ..., e) *)
  | PE_fby of p_expr * p_expr

(* no fby for now *)

type p_patt = { ppatt_desc : p_patt_desc; ppatt_loc : location }
and p_patt_desc = PP_ident of ident | PP_tuple of ident list

type p_equation = { peq_patt : p_patt; peq_expr : p_expr }

type p_node = {
  pn_name : ident;
  pn_input : (ident * base_ty * p_clock) list;
  pn_output : (ident * base_ty * p_clock) list;
  pn_local : (ident * base_ty * p_clock) list;
  pn_equs : p_equation list;
  pn_loc : location;
}

type p_file = p_node list
