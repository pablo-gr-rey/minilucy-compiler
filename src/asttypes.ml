type location = Lexing.position * Lexing.position
type base_ty = Tbool | Tint | Treal
type ty = base_ty list
type const = Cbool of bool | Cint of int | Creal of float | Cnil of base_ty

type op =
  | Op_eq
  | Op_neq
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  | Op_mod
  | Op_add_f
  | Op_sub_f
  | Op_mul_f
  | Op_div_f
  | Op_not
  | Op_and
  | Op_or
  | Op_impl
  | Op_if

type binop =
  | Binop_eq
  | Binop_neq
  | Binop_lt
  | Binop_le
  | Binop_gt
  | Binop_ge
  | Binop_add
  | Binop_sub
  | Binop_mul
  | Binop_div
  | Binop_mod
  | Binop_add_f
  | Binop_sub_f
  | Binop_mul_f
  | Binop_div_f
  | Binop_and
  | Binop_or
  | Binop_impl

type unop = Unop_not | Unop_neg
