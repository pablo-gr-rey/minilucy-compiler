(* new file: describes normalized lustre AST *)

open Asttypes
open Typed_ast

type n_expr = { nexpr_desc : n_expr_desc; nexpr_type : base_ty; nexpr_loc : location }

and n_expr_desc =
  | NE_const of const
  | NE_ident of Ident.t
  | NE_unop of unop * n_expr
  | NE_binop of binop * n_expr * n_expr
  | NE_when of n_expr * bool * Ident.t (* true = when var; false = when not var*)

and n_cexpr = { ncexpr_desc : n_cexpr_desc; ncexpr_type : base_ty; ncexpr_loc : location }

and n_cexpr_desc =
  | NCE_expr of n_expr
  | NCE_merge of Ident.t * n_cexpr * n_cexpr (* merge x (true -> ce1) (false -> ce2) *)
  | NCE_ite of n_expr * n_cexpr * n_cexpr

type n_eq =
  | NEQ_ce of Ident.t * n_cexpr (* x = ce *)
  | NEQ_fby of Ident.t * const * n_expr (* x = c fby e *)
  | NEQ_pre of Ident.t * n_expr (* x = pre e *)
  | NEQ_app of t_patt * Ident.t * n_expr list (* x+ = f(e+) *)
  | NEQ_prim of t_patt * Ident.t * n_expr list (* x+ = f(e+) *)

type n_node = {
  nn_name : Ident.t;
  nn_input : typed_var list;
  nn_output : typed_var list;
  nn_local : typed_var list;
  nn_equs : n_eq list;
  nn_loc : location;
}

type n_file = n_node list
