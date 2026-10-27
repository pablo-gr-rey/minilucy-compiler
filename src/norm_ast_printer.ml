open Format
open Asttypes
open Norm_ast
open Typed_ast
open Typed_ast_printer

let print_unop fmt u =
  let s = match u with Unop_not -> "not" | Unop_neg -> "-" in
  fprintf fmt "%s" s

let print_binop fmt b =
  let s =
    match b with
    | Binop_eq -> "="
    | Binop_neq -> "!="
    | Binop_lt -> "<"
    | Binop_le -> "<="
    | Binop_gt -> ">"
    | Binop_ge -> ">="
    | Binop_add -> "+"
    | Binop_sub -> "-"
    | Binop_mul -> "*"
    | Binop_div -> "/"
    | Binop_mod -> "mod"
    | Binop_add_f -> "+."
    | Binop_sub_f -> "-."
    | Binop_mul_f -> "*."
    | Binop_div_f -> "/."
    | Binop_and -> "and"
    | Binop_or -> "or"
    | Binop_impl -> "=>"
  in
  fprintf fmt "%s" s

let print_when_bool fmt = function true -> fprintf fmt "when" | false -> fprintf fmt "when not"

let rec print_nexpr fmt ne =
  match ne.nexpr_desc with
  | NE_const c -> print_const fmt c
  | NE_ident var -> Ident.print fmt var
  | NE_unop (u, ne) -> fprintf fmt "@[%a (@[%a@])@]" print_unop u print_nexpr ne
  | NE_binop (b, ne1, ne2) ->
      fprintf fmt "@[(@[%a@]) %a (@[%a@])@]" print_nexpr ne1 print_binop b print_nexpr ne2
  | NE_when (ne, b, var) ->
      fprintf fmt "@[(@[%a@]) @[%a@] @[%a@]@]" print_nexpr ne print_when_bool b Ident.print var

let rec print_ncexpr fmt nce =
  match nce.ncexpr_desc with
  | NCE_expr ne -> print_nexpr fmt ne
  | NCE_merge (var, nce1, nce2) ->
      fprintf fmt "@[merge %a (@[true -> @[%a@]@]) (@[false -> @[%a@]@]@])" Ident.print var
        print_ncexpr nce1 print_ncexpr nce2

let print_neq fmt = function
  | NEQ_ce (var, nce) -> fprintf fmt "@[%a = @[%a@]@]" Ident.print var print_ncexpr nce
  | NEQ_fby (var, cst, ne) ->
      fprintf fmt "@[%a = @[%a@] fby @[%a@]@]" Ident.print var print_const cst print_nexpr ne
  | NEQ_app (patt, f, nel) | NEQ_prim (patt, f, nel) ->
      fprintf fmt "@[(%a) = @[%a (@[%a@])@]@]" (print_list Ident.print ",") patt.tpatt_desc
        Ident.print f (print_list print_nexpr ",") nel

let print_node fmt node =
  fprintf fmt "@[node %a(@[%a@]) returns (@[%a@])@\nvar @[%a@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    Ident.print node.nn_name print_var_dec_list node.nn_input print_var_dec_list node.nn_output
    print_var_dec_list node.nn_local (print_list_eol print_neq ";") node.nn_equs

let print_node_list_std = List.iter (fun n -> Format.printf "%a@\n@." print_node n)
