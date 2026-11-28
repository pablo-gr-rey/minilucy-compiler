(* code original d'Adrien Guatto *)
(* modifié par Pablo Rey *)
open Format
open Parse_ast
open Typed_ast

let rec print_list f sep fmt l =
  match l with
  | [] -> ()
  | [ x ] -> f fmt x
  | h :: t -> fprintf fmt "%a%s%a" f h sep (print_list f sep) t

let rec print_list_eol f fmt l =
  match l with
  | [] -> ()
  | [ x ] -> fprintf fmt "%a" f x
  | h :: t -> fprintf fmt "%a@;%a" f h (print_list_eol f) t

let rec print_list_eol_sep f sep fmt l =
  match l with
  | [] -> ()
  | [ x ] -> fprintf fmt "%a" f x
  | h :: t -> fprintf fmt "%a%s@;%a" f h sep (print_list_eol f) t

(* prints several list with their respective functions *)
(* I'm not sure if it's possible to write a general function, as the elements and the printing functions may have different types (so we can't just put them in a list) *)
let rec print_2_lists_aux is_first f1 f2 sep fmt (l1, l2) =
  match (l1, l2) with
  | [], [] -> ()
  | x :: l, _ ->
      fprintf fmt "%s%a%a"
        (if is_first then "" else sep)
        f1 x
        (print_2_lists_aux false f1 f2 sep)
        (l, l2)
  | [], x :: l ->
      fprintf fmt "%s%a%a"
        (if is_first then "" else sep)
        f2 x
        (print_2_lists_aux false f1 f2 sep)
        ([], l)

let print_2_lists f1 f2 sep fmt (l1, l2) = print_2_lists_aux true f1 f2 sep fmt (l1, l2)

let rec print_3_lists_aux is_first f1 f2 f3 sep fmt (l1, l2, l3) =
  match (l1, l2, l3) with
  | [], [], [] -> ()
  | x :: l1', _, _ ->
      fprintf fmt "%s%a%a"
        (if is_first then "" else sep)
        f1 x
        (print_3_lists_aux false f1 f2 f3 sep)
        (l1', l2, l3)
  | [], x :: l2', _ ->
      fprintf fmt "%s%a%a"
        (if is_first then "" else sep)
        f2 x
        (print_3_lists_aux false f1 f2 f3 sep)
        ([], l2', l3)
  | [], [], x :: l3' ->
      fprintf fmt "%s%a%a"
        (if is_first then "" else sep)
        f3 x
        (print_3_lists_aux false f1 f2 f3 sep)
        ([], [], l3')

let print_3_lists f1 f2 f3 sep fmt (l1, l2, l3) =
  print_3_lists_aux true f1 f2 f3 sep fmt (l1, l2, l3)

let print_parse_btype fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Treal -> fprintf fmt "real"

let print_bty : type a. formatter -> a bty -> unit =
 fun fmt bty ->
  match bty with
  | BTY_int -> fprintf fmt "int"
  | BTY_bool -> fprintf fmt "bool"
  | BTY_real -> fprintf fmt "real"

let rec print_nety : type a. formatter -> a nety -> unit =
 fun fmt nty ->
  match nty with
  | NTY_bty bty -> print_bty fmt bty
  | NTY_pair (t1, t2) -> fprintf fmt "(%a, %a)" print_nety t1 print_nety t2

let print_ty : type a. formatter -> a ty -> unit =
 fun fmt ty -> match ty with TY_nty nty -> print_nety fmt nty | TY_unit -> fprintf fmt "unit"

let print_const : type a. formatter -> a const -> unit =
 fun fmt c ->
  match c with
  | TC_int n -> fprintf fmt "%d" n
  | TC_bool b -> fprintf fmt "%b" b
  | TC_real f -> fprintf fmt "%f" f
  | TC_nil b -> fprintf fmt "nil (%a)" print_bty b

let print_op fmt op =
  match op with
  | Op_eq -> fprintf fmt "eq"
  | Op_neq -> fprintf fmt "neq"
  | Op_lt -> fprintf fmt "lt"
  | Op_le -> fprintf fmt "le"
  | Op_gt -> fprintf fmt "gt"
  | Op_ge -> fprintf fmt "ge"
  | Op_add -> fprintf fmt "add"
  | Op_sub -> fprintf fmt "sub"
  | Op_mul -> fprintf fmt "mul"
  | Op_div -> fprintf fmt "div"
  | Op_mod -> fprintf fmt "mod"
  | Op_add_f -> fprintf fmt "add_f"
  | Op_sub_f -> fprintf fmt "sub_f"
  | Op_mul_f -> fprintf fmt "mul_f"
  | Op_div_f -> fprintf fmt "div_f"
  | Op_not -> fprintf fmt "not"
  | Op_and -> fprintf fmt "and"
  | Op_or -> fprintf fmt "or"
  | Op_impl -> fprintf fmt "impl"
  | Op_if -> fprintf fmt "ite"

let op_name = function
  | Op_not -> "~"
  | Op_eq -> "="
  | Op_neq -> "!="
  | Op_lt -> "<"
  | Op_le -> "<="
  | Op_gt -> ">"
  | Op_ge -> ">="
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | Op_mod -> "%"
  | Op_add_f -> "+."
  | Op_sub_f -> "-."
  | Op_mul_f -> "*."
  | Op_div_f -> "/."
  | Op_and -> "&&"
  | Op_or -> "||"
  | Op_impl -> "=>"
  | Op_if -> "if-then-else"

let prim_name : type a b. (a, b) typed_prim -> string = function
  | TYP_not -> "~"
  | TYP_neg -> "-"
  | TYP_neg_f -> "-."
  | TYP_eq -> "="
  | TYP_neq -> "!="
  | TYP_lt -> "<"
  | TYP_le -> "<="
  | TYP_gt -> ">"
  | TYP_ge -> ">="
  | TYP_lt_f -> "<."
  | TYP_le_f -> "<=."
  | TYP_gt_f -> ">."
  | TYP_ge_f -> ">=."
  | TYP_add -> "+"
  | TYP_sub -> "-"
  | TYP_mul -> "*"
  | TYP_div -> "/"
  | TYP_mod -> "%"
  | TYP_add_f -> "+."
  | TYP_sub_f -> "-."
  | TYP_mul_f -> "*."
  | TYP_div_f -> "/."
  | TYP_and -> "&&"
  | TYP_or -> "||"
  | TYP_impl -> "=>"
  | TYP_if -> "ite"
  | TYP_int_of_real -> "int_of_real"
  | TYP_real_of_int -> "real_of_int"
  | TYP_arrow -> "->"
  | TYP_fby -> "fby"
  | TYP_pre -> "pre"
  | TYP_when ((id, _), true) -> "when{" ^ Ident.get_full id ^ "}"
  | TYP_when ((id, _), false) -> "when{not " ^ Ident.get_full id ^ "}"
  | TYP_merge (id, _) -> "merge{" ^ Ident.get_full id ^ "}"

let is_pair : type a. a t_expr_desc -> bool = function TE_pair _ -> true | _ -> false

let rec print_expr : type a. formatter -> a t_expr -> unit =
 fun fmt e ->
  let print_par fmt e =
    if is_pair e.texpr_desc then fprintf fmt "(%a)" print_expr e else print_expr fmt e
  in
  match e.texpr_desc with
  | TE_unit -> fprintf fmt "()"
  | TE_const c -> print_const fmt c
  | TE_ident (x, _) -> fprintf fmt "%a" Ident.print_full x
  | TE_pair (e1, e2) -> fprintf fmt "%a, %a" print_par e1 print_par e2
  | TE_app (TYF_node n, e) -> fprintf fmt "%a@[(%a)@]" Ident.print_full n.tn_name print_expr e
  | TE_app (TYF_prim p, e) -> fprintf fmt "%s@[(%a)@]" (prim_name p) print_expr e

(* if bool is true, we print types, otherwise not *)
let rec print_ne_patt_desc : type a. bool -> formatter -> a t_ne_patt_desc -> unit =
 fun print_ty fmt p ->
  match p with
  | TPNE_var (x, bty) ->
      if print_ty then fprintf fmt "%a: %a" Ident.print_full x print_bty bty
      else fprintf fmt "%a" Ident.print_full x
  | TPNE_cons ((x, bty), patt) ->
      if print_ty then fprintf fmt "%a: %a" Ident.print_full x print_bty bty
      else fprintf fmt "%a" Ident.print_full x;
      fprintf fmt ", %a" (print_ne_patt_desc print_ty) patt.tnepatt_desc

let print_ne_patt print_ty fmt p = print_ne_patt_desc print_ty fmt p.tnepatt_desc

(* print_patt (show_types) (show_parenthesis) *)
let print_patt : type a. bool -> bool -> formatter -> a t_patt -> unit =
 fun print_ty print_par fmt patt ->
  match patt.tpatt_desc with
  | TP_patt (TPNE_cons _ as p) when print_par ->
      fprintf fmt "@[(%a)@]" (print_ne_patt_desc print_ty) p
  | TP_patt p -> print_ne_patt_desc print_ty fmt p
  | TP_unit -> ()

let print_eq fmt = function
  | TQ_eq (patt, expr) -> fprintf fmt "@[%a@] = @[%a@];" (print_ne_patt false) patt print_expr expr

let rec print_clock fmt = function
  | TC_base -> fprintf fmt "base"
  | TC_when (ck, x) ->
      print_clock fmt ck;
      fprintf fmt " when %s" x.name
  | TC_whennot (ck, x) ->
      print_clock fmt ck;
      fprintf fmt " when not %s" x.name

let rec print_clock_list fmt = function
  | [] -> ()
  | [ c ] -> print_clock fmt c
  | c :: cks -> fprintf fmt "%a; %a" print_clock c print_clock_list cks

let print_node : type a b. formatter -> (a, b) t_node -> unit =
 fun fmt nd ->
  let print_var : type a. formatter -> a t_patt -> unit =
   fun fmt vars ->
    match vars.tpatt_desc with
    | TP_patt p -> fprintf fmt "var %a@;" (print_ne_patt_desc true) p
    | TP_unit -> ()
  in
  match nd.tn_desc with
  | TN_desc (input, output, local) ->
      fprintf fmt "@[<v>node %a(@[%a@])@;returns @[(%a)@]@;%alet@[<v 4>@;%a@]@;tel@]"
        Ident.print_full nd.tn_name (print_patt true false) input (print_ne_patt true) output
        print_var local (print_list_eol print_eq) nd.tn_equs

let print_node_list_std ndl =
  List.iter (fun (Box_t_node nd) -> Format.printf "%a@\n@." print_node nd) ndl
