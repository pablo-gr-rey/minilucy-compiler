type location = Lexing.position * Lexing.position

let set_file, report_loc =
  let file = ref "" in
  let set_file s = file := s in
  let report_loc ((b, e) : location) =
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol + 1 in
    let lc = e.pos_cnum - b.pos_bol + 1 in
    Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" !file l fc lc
  in
  (set_file, report_loc)

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

type unop = Unop_not | Unop_neg | Unop_neg_f
