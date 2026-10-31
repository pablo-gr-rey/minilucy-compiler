open Format
open Objc_ast
open Typed_ast_printer
open Norm_ast_printer

(* let rec print_nexpr fmt ne =
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

let print_node_list_std = List.iter (fun n -> Format.printf "%a@\n@." print_node n) *)

let print_tyvar fmt ((id, ty) : oc_var) =
  fprintf fmt "@[<h>%a: %a@]" Ident.print id print_base_type ty

let print_inst_item fmt (id, node) = fprintf fmt "@[<h>%a: %a@]" Ident.print id Ident.print node
let print_vlist = print_list print_tyvar "; "

let rec print_ocexpr fmt = function
  | OCE_const c -> print_const fmt c
  | OCE_var var -> Ident.print fmt var
  | OCE_state var -> fprintf fmt "@[state(@[%a@])@]" Ident.print var
  | OCE_unop (u, oce) -> fprintf fmt "@[%a (@[%a@])@]" print_unop u print_ocexpr oce
  | OCE_binop (b, oce1, oce2) ->
      fprintf fmt "@[(@[%a@]) %a (@[%a@])@]" print_ocexpr oce1 print_binop b print_ocexpr oce2

and print_ocstmt fmt = function
  | OCS_assign (var, oce) -> fprintf fmt "@[@[%a@] = @[%a@]@];" Ident.print var print_ocexpr oce
  | OCS_state_assign (var, oce) ->
      fprintf fmt "@[state(@[%a@]) = @[%a@]@];" Ident.print var print_ocexpr oce
  | OCS_reset var -> fprintf fmt "@[@[%a@].reset()@];" Ident.print var
  | OCS_step (vl, o, el) ->
      fprintf fmt "@[(%a) = @[%a@].step(@[%a@])@];" (print_list Ident.print ",") vl Ident.print o
        (print_list print_ocexpr ",") el
  | OCS_prim (vl, f, el) ->
      fprintf fmt "@[(%a) = @[%a@](@[%a@])@];" (print_list Ident.print ",") vl Ident.print f
        (print_list print_ocexpr ",") el
  | OCS_case (x, stmtT, stmtF) ->
      fprintf fmt "@[<v>if (%a) {@;@[<v 2>  %a@]@;} else {@;@[<v 2>  %a@]@;}@]" Ident.print x
        print_slist stmtT print_slist stmtF

and print_slist fmt sl = print_list_eol print_ocstmt "" fmt sl

let print_def fmt ocd =
  let s_input, s_output, s_local, s_stmts = ocd.ocd_step in
  fprintf fmt
    "@[<v 2>machine %a =@;\
     memory @[<h>(%a)@]@;\
     instances @[<h>(%a)@]@;\
     reset () = {@;\
     @[<v 2>  %a@]@;\
     }@;\
     step(@[<h>%a@]) returns (@[<h>%a@])@;\
     var (@[<h>%a@]) in {@;\
     @[<v 2>  %a@]@;\
     }@]"
    (* "@[machine %a = @\n\
     @[<v 2> memory @[%a@]@\n\
     instances @[%a@]@\n\
     reset () = {\n\
     @[<v 2>%a@]@\n\
     }@\n\
     step(@[%a@]) returns (@[%a@])@\n\
     var @[%a@] in {@\n\
     @[<v 2>%a@]@\n\
     }@]" *)
    Ident.print ocd.ocd_name print_vlist ocd.ocd_mem (print_list print_inst_item "; ") ocd.ocd_insts
    print_slist ocd.ocd_reset print_vlist s_input print_vlist s_output print_vlist s_local
    print_slist s_stmts

let print_def_list_std defs = List.iter (fun d -> Format.printf "%a@\n@." print_def d) defs
