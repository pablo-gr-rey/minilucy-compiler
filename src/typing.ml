open Asttypes
open Parse_ast
open Typed_ast
open Typed_ast_printer
open Format
module S = Set.Make (Ident)
module M = Map.Make (String)

type error =
  | ExpectedBaseType : 'a bty * 'b bty * string -> error (* (has type 'a bty, expected 'b bty) *)
  | ExpectedNeType : 'a nety * 'b nety * string -> error
  | ExpectedType : 'a ty * 'b ty * string -> error
  | UnexpectedUnit
  | UnexpectedBaseType : 'a bty * string -> error
  | UnexpectedNeType : 'a nety * string -> error
  | UnexpectedType : 'a ty * string -> error
  | InvalidPatternType : 'a ty -> error
  | InvalidArity of int
  | InvalidMerge of bool * bool
  | EmptyOutput of string
  | UnboundVar of string
  | UnboundNode of string
  | UndefinedVars of Ident.t list
  | RedefVar of string
  | RedefNode of string
  | DefinedTwice of Ident.t
  | Other of string

exception Error of location * error

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)
let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let warning loc fmt =
  report_loc loc;
  Format.kasprintf (fun s -> eprintf "Warning: %s\n@." s) fmt

let print_op_int = function
  | Op_add -> "+"
  | Op_sub -> "-"
  | Op_mul -> "*"
  | Op_div -> "/"
  | _ -> ""

let print_op_real = function
  | Op_add -> "+."
  | Op_sub -> "-."
  | Op_mul -> "*."
  | Op_div -> "/."
  | _ -> ""

let report fmt = function
  | ExpectedBaseType (bty1, bty2, str) ->
      fprintf fmt "(base) type mismatch while typing %s: got type %a, expected type %a" str
        print_bty bty1 print_bty bty2
  | ExpectedNeType (nty1, nty2, str) ->
      fprintf fmt "(non-empty) type mismatch while typing %s: got type %a, expected type %a" str
        print_nety nty1 print_nety nty2
  | ExpectedType (ty1, ty2, str) ->
      fprintf fmt "type mismatch while typing %s: got type %a, expected type %a" str print_ty ty1
        print_ty ty2
  | UnexpectedUnit -> fprintf fmt "unexpected unit expression"
  | UnexpectedBaseType (bty, str) ->
      fprintf fmt "unexpected (base) type %a while typing %s" print_bty bty str
  | UnexpectedNeType (nety, str) ->
      fprintf fmt "unexpected (non-empty) type %a while typing %s" print_nety nety str
  | UnexpectedType (ty, str) -> fprintf fmt "unexpected type %a while typing %s" print_ty ty str
  | InvalidPatternType ty ->
      begin match ty with
      | TY_unit -> fprintf fmt "this expression has type unit but is part of a pattern"
      | TY_nty nety ->
          fprintf fmt "nested patterns are not allowed (got %a instead of a base type)" print_nety
            nety
      end
  | InvalidArity n -> fprintf fmt "invalid arity for function call (expected %d arguments)" n
  | InvalidMerge (b1, b2) ->
      fprintf fmt "invalid merge: got cases (%b, %b) but expected (true, false) or (false, true)" b1
        b2
  | EmptyOutput n -> fprintf fmt "node %s has no output variable" n
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | UndefinedVars l ->
      fprintf fmt "these output or local variables were not defined: %a"
        (print_list Ident.print "; ") l
  | RedefVar x -> fprintf fmt "variable %s already exists" x
  | RedefNode x -> fprintf fmt "node %s already exists" x
  | DefinedTwice x -> fprintf fmt "variable %a defined in several equations" Ident.print_full x
  | Other s -> fprintf fmt "%s" s

type io = Vinput | Voutput | Vlocal

(* env for the variables *)
module Gamma = struct
  type t = boxed_typed_var M.t

  let empty = M.empty

  let add loc env name var =
    if M.mem name env then error loc (RedefVar name);

    M.add name var env

  let adds loc = List.fold_left (fun env (name, var) -> add loc env name var)
  let find loc env x = try M.find x env with Not_found -> error loc (UnboundVar x)
end

let eq_bty_err : type a b. a bty -> b bty -> location -> string -> (a, b) eq =
 fun x y loc str ->
  match eq_bty x y with None -> error loc (ExpectedBaseType (x, y, str)) | Some Refl -> Refl

let eq_nety_err : type a b. a nety -> b nety -> location -> string -> (a, b) eq =
 fun x y loc str ->
  match eq_nety x y with None -> error loc (ExpectedNeType (x, y, str)) | Some Refl -> Refl

let eq_ty_err : type a b. a ty -> b ty -> location -> string -> (a, b) eq =
 fun x y loc str ->
  match eq_ty x y with None -> error loc (ExpectedType (x, y, str)) | Some Refl -> Refl

(* quite often, the type inferred by OCaml is less general than it should, so we have to add annotations *)
let ty_of_bty : type a. a bty -> a ty = fun bty -> TY_nty (NTY_bty bty)

let nety_of_ty : type a. a ty -> location -> a nety =
 fun ty loc -> match ty with TY_nty nety -> nety | _ -> error loc UnexpectedUnit

let ty_of_pair a b = TY_nty (NTY_pair (NTY_bty a, NTY_bty b))

let bty_of_pty = function
  | Tint -> Box_bty BTY_int
  | Treal -> Box_bty BTY_real
  | Tbool -> Box_bty BTY_bool

module Prims = struct
  (* we have to create runtime specifications for the functions to store runtime descriptions of the input and output types (for the simple information of e.g.   | TYP_eq : ('a * 'a, bool) typed_prim we cannot extract a function that verifies that the input type is TY_nty (NTY_pair (ne, ne)) and builds a runtime description of the return type, so we do that here *)
  (* since we only have a few polymorphic primitives, we handle them by hand (e.g. a specification that takes the unknown argument and returns the corresponding spec) and not in a general way, but we'll have to do that for clocks anyway *)
  let box func input_ty output_ty = Box_fun_spec { func = TYF_prim func; input_ty; output_ty }

  (* unop *)
  let not_spec = box TYP_not (ty_of_bty BTY_bool) (ty_of_bty BTY_bool)
  let neg_spec = box TYP_neg (ty_of_bty BTY_int) (ty_of_bty BTY_int)
  let neg_f_spec = box TYP_neg_f (ty_of_bty BTY_real) (ty_of_bty BTY_real)

  (* equality (polymorphic) *)
  let eq_spec nety = box TYP_eq (TY_nty (NTY_pair (nety, nety))) (ty_of_bty BTY_bool)
  let neq_spec nety = box TYP_neq (TY_nty (NTY_pair (nety, nety))) (ty_of_bty BTY_bool)

  (* comparisons (may work on integer and real) *)
  let lt_spec : type a. a nety -> location -> boxed_fun_spec =
   fun nety loc ->
    match nety with
    | NTY_pair (NTY_bty BTY_int, _) -> box TYP_lt (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_bool)
    | NTY_pair (NTY_bty BTY_real, _) ->
        box TYP_lt_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_bool)
    | _ -> error loc (UnexpectedNeType (nety, "operator <"))

  let le_spec : type a. a nety -> location -> boxed_fun_spec =
   fun nety loc ->
    match nety with
    | NTY_pair (NTY_bty BTY_int, _) -> box TYP_le (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_bool)
    | NTY_pair (NTY_bty BTY_real, _) ->
        box TYP_le_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_bool)
    | _ -> error loc (UnexpectedNeType (nety, "operator <="))

  let gt_spec : type a. a nety -> location -> boxed_fun_spec =
   fun nety loc ->
    match nety with
    | NTY_pair (NTY_bty BTY_int, _) -> box TYP_gt (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_bool)
    | NTY_pair (NTY_bty BTY_real, _) ->
        box TYP_gt_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_bool)
    | _ -> error loc (UnexpectedNeType (nety, "operator >"))

  let ge_spec : type a. a nety -> location -> boxed_fun_spec =
   fun nety loc ->
    match nety with
    | NTY_pair (NTY_bty BTY_int, _) -> box TYP_ge (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_bool)
    | NTY_pair (NTY_bty BTY_real, _) ->
        box TYP_ge_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_bool)
    | _ -> error loc (UnexpectedNeType (nety, "operator >="))

  (* arithmetic (integer and real) (sub may be unary or binary!) *)
  let add_spec = box TYP_add (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_int)

  let sub_spec : type a. a nety -> boxed_fun_spec =
   fun nety ->
    match nety with
    | NTY_bty _ -> box TYP_neg (ty_of_bty BTY_int) (ty_of_bty BTY_int)
    | _ -> box TYP_sub (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_int)
  (* if this is not the right type, it will be caught later. we just make an arbitrary choice between unop sub and binop sub for ill-typed terms *)

  let mul_spec = box TYP_mul (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_int)
  let div_spec = box TYP_div (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_int)
  let mod_spec = box TYP_mod (ty_of_pair BTY_int BTY_int) (ty_of_bty BTY_int)
  let add_f_spec = box TYP_add_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_real)

  let sub_f_spec : type a. a nety -> boxed_fun_spec =
   fun nety ->
    match nety with
    | NTY_bty _ -> box TYP_neg_f (ty_of_bty BTY_real) (ty_of_bty BTY_real)
    | _ -> box TYP_sub_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_real)

  let mul_f_spec = box TYP_mul_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_real)
  let div_f_spec = box TYP_div_f (ty_of_pair BTY_real BTY_real) (ty_of_bty BTY_real)

  (* boolean operators *)
  let and_spec = box TYP_and (ty_of_pair BTY_bool BTY_bool) (ty_of_bty BTY_bool)
  let or_spec = box TYP_or (ty_of_pair BTY_bool BTY_bool) (ty_of_bty BTY_bool)
  let impl_spec = box TYP_impl (ty_of_pair BTY_bool BTY_bool) (ty_of_bty BTY_bool)

  (* if *)
  let if_spec nety =
    box TYP_if (TY_nty (NTY_pair (NTY_bty BTY_bool, NTY_pair (nety, nety)))) (TY_nty nety)

  (* conversions *)
  let int_of_real_spec = box TYP_int_of_real (ty_of_bty BTY_real) (ty_of_bty BTY_int)
  let real_of_int_spec = box TYP_real_of_int (ty_of_bty BTY_int) (ty_of_bty BTY_real)

  (* clock primitives *)
  let arrow_spec nety = box TYP_arrow (TY_nty (NTY_pair (nety, nety))) (TY_nty nety)
  let fby_spec nety = box TYP_fby (TY_nty (NTY_pair (nety, nety))) (TY_nty nety)
  let pre_spec nety = box TYP_pre (TY_nty nety) (TY_nty nety)
  let when_spec var b nety = box (TYP_when (var, b)) (TY_nty nety) (TY_nty nety)
  let merge_spec var nety = box (TYP_merge var) (TY_nty (NTY_pair (nety, nety))) (TY_nty nety)
end

let rec type_expr_list nodes env loc exprs =
  let mk : type a. a t_expr_desc -> a ty -> boxed_t_expr =
   fun ted ty -> Box_t_expr { texpr_desc = ted; texpr_ty = ty; texpr_loc = loc }
  in

  match exprs with
  | [] -> mk TE_unit TY_unit
  | [ pe ] -> type_expr nodes env pe
  | pe :: pel ->
      (* we build a canonical representation NTY_pair (NTY_bty bt1, NTY_pair (...)) *)
      let (Box_t_expr te1) = type_expr nodes env pe in
      let (Box_t_expr te2) = type_expr_list nodes env loc pel in
      mk
        (TE_pair (te1, te2))
        (TY_nty (NTY_pair (nety_of_ty te1.texpr_ty loc, nety_of_ty te2.texpr_ty loc)))

and type_expr nodes env pexpr =
  let loc = pexpr.pexpr_loc in
  let mk : type a. a t_expr_desc -> a ty -> boxed_t_expr =
   fun ted ty -> Box_t_expr { texpr_desc = ted; texpr_ty = ty; texpr_loc = loc }
  in
  let type_expr_ = type_expr nodes env in

  (* check that the expression has the input type of the function, and build a fun of the output type of the function *)
  let mk_fun : type a. a t_expr -> boxed_fun_spec -> string -> boxed_t_expr =
   (* we use the runtime description of the input & return types provided by the spec *)
   fun te (Box_fun_spec spec) str ->
    let Refl = eq_ty_err te.texpr_ty spec.input_ty loc ("arguments of " ^ str) in
    Box_t_expr { texpr_desc = TE_app (spec.func, te); texpr_ty = spec.output_ty; texpr_loc = loc }
  in

  let error_ err = error loc err in

  match pexpr.pexpr_desc with
  | PE_const c ->
      begin match c with
      | PC_int i -> mk (TE_const (TC_int i)) (ty_of_bty BTY_int)
      | PC_bool b -> mk (TE_const (TC_bool b)) (ty_of_bty BTY_bool)
      | PC_real b -> mk (TE_const (TC_real b)) (ty_of_bty BTY_real)
      end
  | PE_ident name ->
      let (Box_typed_var var) = Gamma.find loc env name in
      let _, bty = var in
      mk (TE_ident var) (ty_of_bty bty)
  | PE_op (op, pel) ->
      (* we could also provide handle polymorphism in a general way, as a fun extension of the project, but since here it's only a couple of primitives (eq, neq, if and clock) we can do it by hand *)
      let (Box_t_expr args) = type_expr_list nodes env loc pel in
      let nety = nety_of_ty args.texpr_ty loc in
      let mk_prim spec = mk_fun args spec ("operator " ^ op_name op) in

      (* if the type of args is 'a * 'a, return 'a (as runtime desc) (for eq and fby) *)
      (* we do not check that both part of the pairs are equal, as this is done later in the typing *)
      let get_pair_type () =
        match nety with NTY_pair (ne1, _) -> Box_nety ne1 | _ -> error_ (InvalidArity 2)
      in
      (* if the type of args is 'a * ('b * 'b), return 'b (does not check equality) (for if and merge) *)
      let get_snd_pair_type () =
        match nety with
        | NTY_pair (_, NTY_pair (ne2, _)) -> Box_nety ne2
        | _ -> error_ (InvalidArity 3)
      in
      begin match op with
      | Op_eq ->
          let (Box_nety nety) = get_pair_type () in
          (* mk_prim checks that the two arguments have the same type *)
          mk_prim (Prims.eq_spec nety)
      | Op_neq ->
          let (Box_nety nety) = get_pair_type () in
          mk_prim (Prims.neq_spec nety)
      | Op_lt -> mk_prim (Prims.lt_spec nety loc) (* comparison might take int*int or float*float *)
      | Op_le -> mk_prim (Prims.le_spec nety loc)
      | Op_gt -> mk_prim (Prims.gt_spec nety loc)
      | Op_ge -> mk_prim (Prims.ge_spec nety loc)
      | Op_add -> mk_prim Prims.add_spec
      | Op_sub -> mk_prim (Prims.sub_spec nety) (* might be unary or binary *)
      | Op_mul -> mk_prim Prims.mul_spec
      | Op_div -> mk_prim Prims.div_spec
      | Op_mod -> mk_prim Prims.mod_spec
      | Op_add_f -> mk_prim Prims.add_f_spec
      | Op_sub_f -> mk_prim (Prims.sub_f_spec nety)
      | Op_mul_f -> mk_prim Prims.mul_f_spec
      | Op_div_f -> mk_prim Prims.div_f_spec
      | Op_not -> mk_prim Prims.not_spec
      | Op_and -> mk_prim Prims.and_spec
      | Op_or -> mk_prim Prims.or_spec
      | Op_impl -> mk_prim Prims.impl_spec
      | Op_if ->
          let (Box_nety nety) = get_snd_pair_type () in
          mk_prim (Prims.if_spec nety)
      end
  | PE_app (name, pel) ->
      let (Box_t_expr args) = type_expr_list nodes env loc pel in

      (* name can be either a primitive or a node *)
      if name = "int_of_real" || name = "int" then
        mk_fun args Prims.int_of_real_spec "primitive int_of_real"
      else if name = "real_of_int" || name = "real" then
        mk_fun args Prims.real_of_int_spec "primitive real_of_int"
      else
        let (Box_t_node node) =
          match M.find_opt name nodes with None -> error_ (UnboundNode name) | Some node -> node
        in
        let (TN_desc (intput_patt, output_patt, _)) = node.tn_desc in
        let node_spec =
          Box_fun_spec
            {
              func = TYF_node node;
              input_ty = intput_patt.tpatt_type;
              output_ty = TY_nty output_patt.tnepatt_type;
            }
        in
        mk_fun args node_spec ("node " ^ node.tn_name.name)
  | PE_arrow (pe1, pe2) ->
      let (Box_t_expr e1) = type_expr_ pe1 in
      let (Box_t_expr e2) = type_expr_ pe2 in
      let nety1 = nety_of_ty e1.texpr_ty loc and nety2 = nety_of_ty e2.texpr_ty loc in
      mk_fun
        {
          texpr_desc = TE_pair (e1, e2);
          texpr_ty = TY_nty (NTY_pair (nety1, nety2));
          texpr_loc = loc;
        }
        (Prims.arrow_spec nety1) "operator ->"
  | PE_pre pe ->
      let (Box_t_expr e) = type_expr_ pe in
      let nety = nety_of_ty e.texpr_ty loc in
      mk_fun e (Prims.pre_spec nety) "operator pre"
  | PE_tuple el ->
      (* we build the canonical representation : NTY_pair (e1, NTY_pair (e2, NTY_pair ...)) *)
      (* without adding a unit at the end, unless the list is empty *)
      let rec handle = function
        | [] -> mk TE_unit TY_unit
        | [ pe ] -> type_expr_ pe
        | pe :: pel ->
            let (Box_t_expr e) = type_expr_ pe in
            let (Box_t_expr el) = handle pel in
            (* neither e nor el must have unit type, otherwise the tuple is ill-formed *)
            mk
              (TE_pair (e, el))
              (TY_nty (NTY_pair (nety_of_ty e.texpr_ty loc, nety_of_ty el.texpr_ty loc)))
      in
      handle el
  | PE_when (pe, b, name) ->
      (* thanks to GADTs, this is very quick and we get a nicely typed AST! *)
      let (Box_typed_var var) = Gamma.find loc env name in
      let Refl = eq_bty_err (snd var) BTY_bool loc "operator when" in
      let (Box_t_expr te) = type_expr_ pe in
      mk_fun te (Prims.when_spec var b (nety_of_ty te.texpr_ty loc)) "operator when"
  | PE_merge (name, b1, pe1, b2, pe2) ->
      (* merge x (b1 -> e, ..., e) (b2 -> e, ..., e) *)
      let (Box_typed_var var) = Gamma.find loc env name in
      let Refl = eq_bty_err (snd var) BTY_bool loc "operator merge" in
      let (Box_t_expr e1) = type_expr_ pe1 in
      let (Box_t_expr e2) = type_expr_ pe2 in
      let nety1 = nety_of_ty e1.texpr_ty loc and nety2 = nety_of_ty e2.texpr_ty loc in
      begin match (b1, b2) with
      | true, false ->
          mk_fun
            {
              texpr_desc = TE_pair (e1, e2);
              texpr_ty = TY_nty (NTY_pair (nety1, nety2));
              texpr_loc = loc;
            }
            (Prims.merge_spec var nety1) "operator merge"
      | false, true ->
          (* we swap e2 and e1 (thanks to GADT, ocaml won't type if we also forget to swap nety2 and nety1) *)
          mk_fun
            {
              texpr_desc = TE_pair (e2, e1);
              texpr_ty = TY_nty (NTY_pair (nety2, nety1));
              texpr_loc = loc;
            }
            (Prims.merge_spec var nety1) "operator merge"
      | _ -> error_ (InvalidMerge (b1, b2))
      end
  | PE_fby (pe1, pe2) ->
      let (Box_t_expr e1) = type_expr_ pe1 in
      let (Box_t_expr e2) = type_expr_ pe2 in
      let nety1 = nety_of_ty e1.texpr_ty loc and nety2 = nety_of_ty e2.texpr_ty loc in
      mk_fun
        {
          texpr_desc = TE_pair (e1, e2);
          texpr_ty = TY_nty (NTY_pair (nety1, nety2));
          texpr_loc = loc;
        }
        (Prims.fby_spec nety1) "operator fby"

(* non-empty pattern for equations *)
let type_ppatt env patt =
  let loc = patt.ppatt_loc in
  let mk ne_patt nety =
    Box_t_ne_patt { tnepatt_desc = ne_patt; tnepatt_type = nety; tnepatt_loc = loc }
  in
  match patt.ppatt_desc with
  | PP_ident id ->
      let (Box_typed_var var) = Gamma.find loc env id in
      let _, bty = var in
      mk (TPNE_var var) (NTY_bty bty)
  | PP_tuple idl ->
      let rec handle = function
        | [] -> error loc UnexpectedUnit
        | [ id ] ->
            let (Box_typed_var var) = Gamma.find loc env id in
            let _, bty = var in
            mk (TPNE_var var) (NTY_bty bty)
        | id :: idl ->
            let (Box_typed_var var) = Gamma.find loc env id in
            let _, bty = var in
            let (Box_t_ne_patt tpatt) = handle idl in
            mk (TPNE_cons (var, tpatt)) (NTY_pair (NTY_bty bty, tpatt.tnepatt_type))
      in
      handle idl

let type_equation nodes env eq =
  let (Box_t_ne_patt patt) = type_ppatt env eq.peq_patt in
  let (Box_t_expr expr) = type_expr nodes env eq.peq_expr in
  let Refl = eq_ty_err expr.texpr_ty (TY_nty patt.tnepatt_type) patt.tnepatt_loc "equation" in
  TQ_eq (patt, expr)

let build_var env loc name pty =
  let (Box_bty bty) = bty_of_pty pty in
  let ty_var = Box_typed_var (Ident.make name Ident.Stream, bty) in
  (Gamma.add loc env name ty_var, ty_var)

(* to type input/output/temp lists (returns both the updated env and the patt) *)
let rec build_nepatt env node_name loc patt =
  let mk ne_patt nety =
    Box_t_ne_patt { tnepatt_desc = ne_patt; tnepatt_type = nety; tnepatt_loc = loc }
  in
  match patt with
  | [] -> error loc (EmptyOutput node_name)
  | [ (name, pty, _) ] ->
      let env, Box_typed_var var = build_var env loc name pty in
      (env, mk (TPNE_var var) (NTY_bty (snd var)))
  | (name, pty, _) :: p ->
      let env, Box_typed_var var = build_var env loc name pty in
      let env, Box_t_ne_patt tpatt = build_nepatt env node_name loc p in
      (env, mk (TPNE_cons (var, tpatt)) (NTY_pair (NTY_bty (snd var), tpatt.tnepatt_type)))

let build_patt env node_name loc l =
  let mk patt ty = Box_t_patt { tpatt_desc = patt; tpatt_type = ty; tpatt_loc = loc } in
  match l with
  | [] -> (env, mk TP_unit TY_unit)
  | l ->
      let env, Box_t_ne_patt nep = build_nepatt env node_name loc l in
      (env, mk (TP_patt nep.tnepatt_desc) (TY_nty nep.tnepatt_type))

let check_outputs loc equs temp output =
  let add id set =
    if S.mem id set then error loc (DefinedTwice id);
    S.add id set
  in
  let rec extract_ne_patt : type a. S.t -> a t_ne_patt_desc -> S.t =
   fun set nep ->
    match nep with
    | TPNE_var (id, _) -> add id set
    | TPNE_cons ((id, _), nepp) -> extract_ne_patt (add id set) nepp.tnepatt_desc
  in
  let extract_patt : type a. S.t -> a t_patt -> S.t =
   fun set p -> match p.tpatt_desc with TP_unit -> set | TP_patt nep -> extract_ne_patt set nep
  in
  let vars = extract_ne_patt (extract_patt S.empty temp) output.tnepatt_desc in
  let defined =
    List.fold_left (fun set (TQ_eq (nep, _)) -> extract_ne_patt set nep.tnepatt_desc) S.empty equs
  in
  let not_defined = S.diff vars defined in
  if not (S.is_empty not_defined) then error loc (UndefinedVars (S.elements not_defined))

(* TODO: for now, we do not consider clock annotations, so we emit a warning if we encounter some *)
let check_base_clock =
  let warning_emitted = ref false in
  fun loc (id, _, ck) ->
    if ck <> PC_base && not !warning_emitted then begin
      warning_emitted := true;
      report_loc loc;
      eprintf
        "Warning: clock annotations are currently not taken into account and are not checked \
         against the inferred clock (for variable %s). This warning will be only shown once\n\
         @."
        id
    end

let type_node nodes n =
  (* build env and input/temp/output patterns *)
  let env, Box_t_patt input_patt = build_patt M.empty n.pn_name n.pn_loc n.pn_input in
  let env, Box_t_patt temp_patt = build_patt env n.pn_name n.pn_loc n.pn_local in
  let env, Box_t_ne_patt output_nepatt = build_nepatt env n.pn_name n.pn_loc n.pn_output in

  (* build equations and check that every output & temp var is defined exactly once *)
  let equs = List.map (type_equation nodes env) n.pn_equs in
  check_outputs n.pn_loc equs temp_patt output_nepatt;

  let name = Ident.make n.pn_name Ident.Node in

  (* emit a warning if clock annotations are found *)
  List.iter (check_base_clock n.pn_loc) n.pn_input;
  List.iter (check_base_clock n.pn_loc) n.pn_local;
  List.iter (check_base_clock n.pn_loc) n.pn_output;

  (* causality check & scheduling is now done after normalization, so we can return the node *)
  let node =
    Box_t_node
      {
        tn_name = name;
        tn_desc = TN_desc (input_patt, output_nepatt, temp_patt);
        tn_equs = equs;
        tn_loc = n.pn_loc;
      }
  in
  if M.mem n.pn_name nodes then error n.pn_loc (RedefNode n.pn_name);
  (M.add n.pn_name node nodes, node)

let type_file f =
  let type_aux (env_nodes, node_l) n =
    let env_nodes, new_n = type_node env_nodes n in
    (env_nodes, new_n :: node_l)
  in
  let _, nodes = List.fold_left type_aux (M.empty, []) f in
  List.rev nodes
