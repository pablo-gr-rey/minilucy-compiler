(* new file: describes normalized lustre AST *)

open Asttypes
open Typed_ast

(* type typed_var = Ident.t * base_ty

type t_expr = {
  texpr_desc : t_expr_desc;
  texpr_type : ty;
  texpr_loc : location;
}

and t_expr_desc =
  | TE_const of const
  | TE_ident of Ident.t
  | TE_op of op * t_expr list
  | TE_app of Ident.t * t_expr list
  | TE_prim of Ident.t * t_expr list
  | TE_arrow of t_expr * t_expr
  | TE_pre of t_expr
  | TE_tuple of t_expr list

type t_patt = {
  tpatt_desc : Ident.t list;
  tpatt_type : ty;
  tpatt_loc : location;
}

type t_equation = { teq_patt : t_patt; teq_expr : t_expr }

type t_node = {
  tn_name : Ident.t;
  tn_input : typed_var list;
  tn_output : typed_var list;
  tn_local : typed_var list;
  tn_equs : t_equation list;
  tn_loc : location;
}

type t_file = t_node list *)

type n_expr = {
  nexpr_desc : n_expr_desc;
  nexpr_type : base_ty;
  nexpr_loc : location;
}

and n_expr_desc =
  | NE_const of const
  | NE_ident of Ident.t
  | NE_unop of unop * n_expr
  | NE_binop of binop * n_expr * n_expr
(* | NE_when of n_expr *  *)
