(* Arbres de syntaxe abstraite typés *)

open Asttypes

(* we split types between base types (int, bool or float) and pattern types (base type or base type * pattern) *)
(* the other option is to add pairs as base types, but this means that we can construct (int * int) * int and int * (int * int) as different types *)
(* the issue with this is that we are limited in the nested function calls: if f returns a pair, we can write (x, y, z) = (1, f()) but not (f(), 1) even though heptagon accepts both syntaxes *)
(* but as long as we do not use such syntax (we call a function with either base types or one single function calls, e.g. g(f()) or g(1, 2) if g takes 2 arguments) this works well enough *)
(* TODO: think about this (flattening with witness?...) *)

(* update: now, we can build both (int * int) * int and int * (int * int) as distinct types, but we keep the latter as "canonical" (i.e. the way we type a tuple (1, 2, 3)) *)

(* base types *)
type _ bty = BTY_int : int bty | BTY_bool : bool bty | BTY_real : float bty
and boxed_bty = Box_bty : 'a bty -> boxed_bty

(* non-empty pattern type *)
and _ nety = NTY_bty : 'a bty -> 'a nety | NTY_pair : 'a nety * 'b nety -> ('a * 'b) nety
and boxed_nety = Box_nety : 'a nety -> boxed_nety

(* possibly empty pattern type *)
and _ ty = TY_nty : 'a nety -> 'a ty | TY_unit : unit ty

(* we need equality witnesses to compare types *)
(* equality functions are defined below *)
type (_, _) eq = Refl : ('a, 'a) eq

(* unit is only used to type a null function call (e.g. f()), it should never appear anywhere else as nodes must have at least one output. this is a bit long, but it ensures that every valid type has exactly one inhabitant, otherwise you could have int and or (int * unit). 
before, I had a single pattern type, which was simpler but it had this issue *)

type 'a typed_var = Ident.t * 'a bty
and boxed_typed_var = Box_typed_var : 'a typed_var -> boxed_typed_var

and _ const =
  | TC_int : int -> int const
  | TC_bool : bool -> bool const
  | TC_real : float -> float const
  | TC_nil : 'a bty -> 'a const

and t_clock = TC_base | TC_when of t_clock * Ident.t | TC_whennot of t_clock * Ident.t

(* now, primitives are very elegantly described, and are only described here instead of partly in typing, normalization, conversion, etc... and types match by construction *)
and (_, _) typed_prim =
  (* unop *)
  | TYP_not : (bool, bool) typed_prim
  | TYP_neg : (int, int) typed_prim
  | TYP_neg_f : (float, float) typed_prim
  (* binop *)
  | TYP_eq : ('a * 'a, bool) typed_prim
  | TYP_neq : ('a * 'a, bool) typed_prim
  | TYP_lt : (int * int, bool) typed_prim
  | TYP_le : (int * int, bool) typed_prim
  | TYP_gt : (int * int, bool) typed_prim
  | TYP_ge : (int * int, bool) typed_prim
  | TYP_lt_f : (float * float, bool) typed_prim
  | TYP_le_f : (float * float, bool) typed_prim
  | TYP_gt_f : (float * float, bool) typed_prim
  | TYP_ge_f : (float * float, bool) typed_prim
  | TYP_add : (int * int, int) typed_prim
  | TYP_sub : (int * int, int) typed_prim
  | TYP_mul : (int * int, int) typed_prim
  | TYP_div : (int * int, int) typed_prim
  | TYP_mod : (int * int, int) typed_prim
  | TYP_add_f : (float * float, float) typed_prim
  | TYP_sub_f : (float * float, float) typed_prim
  | TYP_mul_f : (float * float, float) typed_prim
  | TYP_div_f : (float * float, float) typed_prim
  | TYP_and : (bool * bool, bool) typed_prim
  | TYP_or : (bool * bool, bool) typed_prim
  | TYP_impl : (bool * bool, bool) typed_prim
  (* if *)
  | TYP_if : (bool * ('a * 'a), 'a) typed_prim
  (* prims *)
  | TYP_int_of_real : (float, int) typed_prim
  | TYP_real_of_int : (int, float) typed_prim
  (* standard functions on clocks *)
  | TYP_arrow : ('a * 'a, 'a) typed_prim
  | TYP_fby : ('a * 'a, 'a) typed_prim
  | TYP_pre : ('a, 'a) typed_prim
  | TYP_when : bool typed_var * bool -> ('a, 'a) typed_prim
    (* we can even make sure that the when & merge are on boolean variables :) true = when; false = when not *)
  | TYP_merge :
      bool typed_var
      -> ('a * 'a, 'a) typed_prim (* merge var (true => e1) (false => e2) *)

and (_, _) typed_func =
  (* a function is either a node or a primitive *)
  | TYF_node : ('a, 'b) t_node -> ('a, 'b) typed_func
  | TYF_prim : ('a, 'b) typed_prim -> ('a, 'b) typed_func

(* to handle primitives and node generically, we need to specify runtime types as well *)
and ('a, 'b) fun_spec = { func : ('a, 'b) typed_func; input_ty : 'a ty; output_ty : 'b ty }
and boxed_fun_spec = Box_fun_spec : ('a, 'b) fun_spec -> boxed_fun_spec

and _ t_expr_desc =
  | TE_unit : unit t_expr_desc
  | TE_const : 'a const -> 'a t_expr_desc
  | TE_ident : 'a typed_var -> 'a t_expr_desc
  | TE_pair : 'a t_expr * 'b t_expr -> ('a * 'b) t_expr_desc
  | TE_app : ('a, 'b) typed_func * 'a t_expr -> 'b t_expr_desc

and 'a t_expr = { texpr_desc : 'a t_expr_desc; texpr_ty : 'a ty; texpr_loc : location }

(* having to box things is a bit annoying, but that's because we can't write a function returning an existential type directly *)
(* e.g. we would like to do essentially let type_expr : env -> pexpr -> exists a, a t_expr, so we write type_expr : env -> pexpr -> boxed_t_expr which we have to match later *)
(* kind of annoying, but GADT are still worth it *)
and boxed_t_expr = Box_t_expr : 'a t_expr -> boxed_t_expr

and _ t_ne_patt_desc =
  | TPNE_var : 'a typed_var -> 'a t_ne_patt_desc
  | TPNE_cons : 'a typed_var * 'b t_ne_patt -> ('a * 'b) t_ne_patt_desc

and 'a t_ne_patt = {
  tnepatt_desc : 'a t_ne_patt_desc;
  tnepatt_type : 'a nety;
  tnepatt_loc : location;
}

and boxed_t_ne_patt = Box_t_ne_patt : 'a t_ne_patt -> boxed_t_ne_patt
and _ t_patt_desc = TP_patt : 'a t_ne_patt_desc -> 'a t_patt_desc | TP_unit : unit t_patt_desc
and 'a t_patt = { tpatt_desc : 'a t_patt_desc; tpatt_type : 'a ty; tpatt_loc : location }
and boxed_t_patt = Box_t_patt : 'a t_patt -> boxed_t_patt

(* we box equations types as existentials; equations should use non-empty patterns *)
and t_equation = TQ_eq : 'a t_ne_patt * 'a t_expr -> t_equation

(* for a node, we know the input and output types, but the local vars are stored as existential! *)
(* thanks to our little non-empty trick, we can also ensure that all nodes have at least one output, but input and temps can be empty *)
(* (input, output) t_node_desc = T_node_desc (input, output, temp) *)
and (_, _) t_node_desc = TN_desc : 'a t_patt * 'b t_ne_patt * 'c t_patt -> ('a, 'b) t_node_desc

and ('a, 'b) t_node = {
  tn_name : Ident.t;
  tn_desc : ('a, 'b) t_node_desc;
  tn_equs : t_equation list;
  tn_loc : location;
}

and boxed_t_node = Box_t_node : ('a, 'b) t_node -> boxed_t_node

let eq_bty : type a b. a bty -> b bty -> (a, b) eq option =
 fun x y ->
  match (x, y) with
  | BTY_int, BTY_int -> Some Refl
  | BTY_bool, BTY_bool -> Some Refl
  | BTY_real, BTY_real -> Some Refl
  | _ -> None

let rec eq_nety : type a b. a nety -> b nety -> (a, b) eq option =
 fun x y ->
  match (x, y) with
  | NTY_bty bx, NTY_bty by -> eq_bty bx by
  | NTY_pair (tx1, tx2), NTY_pair (ty1, ty2) ->
      begin match (eq_nety tx1 ty1, eq_nety tx2 ty2) with
      | Some Refl, Some Refl -> Some Refl (* ocaml type system magic *)
      | _ -> None
      end
  | _ -> None

let eq_ty : type a b. a ty -> b ty -> (a, b) eq option =
 fun x y ->
  match (x, y) with
  | TY_nty tx, TY_nty ty -> eq_nety tx ty
  | TY_unit, TY_unit -> Some Refl
  | _ -> None
