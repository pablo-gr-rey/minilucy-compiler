open Asttypes
open Typed_ast
open Norm_ast

exception NormException of location * string

let error loc s = raise (NormException (loc, s))

(* why doesn't ocaml have these super useful functions in its stdlib?? *)
(* what do you mean, this isn't very idiomatic? *)
(* TODO: build arbitrary arity functions? *)
let rec map3 f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> []
  | e1 :: l1', e2 :: l2', e3 :: l3' -> f e1 e2 e3 :: map3 f l1' l2' l3'
  | _ -> failwith "invalid map3 call: lists have different lengths"

let rec map4 f l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
  | [], [], [], [] -> []
  | e1 :: l1', e2 :: l2', e3 :: l3', e4 :: l4' -> f e1 e2 e3 e4 :: map4 f l1' l2' l3' l4'
  | _ -> failwith "invalid map4 call: lists have different lengths"

let rec iter3 f l1 l2 l3 =
  match (l1, l2, l3) with
  | [], [], [] -> ()
  | e1 :: l1', e2 :: l2', e3 :: l3' ->
      f e1 e2 e3;
      iter3 f l1' l2' l3'
  | _ -> failwith "invalid iter4 call: lists have different lengths"

let make_dummy_const = function Tbool -> Cbool false | Tint -> Cint 0 | Treal -> Creal 0.0

let unpack = function
  | [ e ] -> e
  | l -> failwith (Printf.sprintf "invalid unpack: expected 1 element, got %d" (List.length l))

let rec texpr_to_nexpr (locals_ref : typed_var list ref) (equs_ref : n_eq list ref) expr :
    n_expr list =
  (* from a single texpr of type [t1, ..., tk], returns a list of texprs [e1: t1, ..., ek: tk] containing no TE_tuple, TE_app or TE_prim inside *)
  let make_nexpr nexpr_d ty =
    { nexpr_desc = nexpr_d; nexpr_type = ty; nexpr_loc = expr.texpr_loc }
  in
  let make_ncexpr ncexpr_d ty =
    { ncexpr_desc = ncexpr_d; ncexpr_type = ty; ncexpr_loc = expr.texpr_loc }
  in
  let error_ s = error expr.texpr_loc s in

  let make_1_nexpr nexpr_d =
    match expr.texpr_type with
    | [ ty ] -> make_nexpr nexpr_d ty
    | _ -> error_ "expected only 1 type"
  in

  let make_temp_var ty : typed_var =
    let n_var = (Ident.make_temp Ident.Stream, ty, TC_unknown) in
    locals_ref := n_var :: !locals_ref;
    n_var
  in

  let var_list_to_patt vl : t_patt =
    {
      tpatt_desc = List.map (fun (n, _, _) -> n) vl;
      tpatt_type = List.map (fun (_, t, _) -> t) vl;
      tpatt_clock = List.map (fun (_, _, c) -> c) vl;
      tpatt_loc = expr.texpr_loc;
    }
  in

  (* laziness... *)
  let t2ne = texpr_to_nexpr locals_ref equs_ref in
  let t2nce = texpr_to_ncexpr locals_ref equs_ref in

  (* convenient functions to handle operators: op1 takes e.g. neg ([te_1, ..., te_k]) and returns [neg ne_1, ..., neg ne_k] *)
  (* op2 similarly takes e.g. add [[te_1, ..., te_k], [te_1', ..., te_k']] and returns [add ne_1 ne_1', ..., add ne_k ne_k'] *)
  let op1 (u : unop) (el : t_expr list) (ty : ty) =
    match el with
    | [ e ] -> List.map2 (fun ne t -> make_nexpr (NE_unop (u, ne)) t) (t2ne e) ty
    | _ -> error_ "invalid arity for unary operator (expected 1)"
  in

  let op2 (b : binop) (el : t_expr list) (ty : ty) =
    match el with
    | [ e1; e2 ] ->
        map3 (fun ne1 ne2 t -> make_nexpr (NE_binop (b, ne1, ne2)) t) (t2ne e1) (t2ne e2) ty
    | _ -> error_ "invalid arity for binary operator (expected 2)"
  in

  let op_if (el : t_expr list) (ty : ty) =
    (* since we have to build an expr, we have to put the if in a new equation and introduce temp variables :( *)
    match el with
    | [ cond; e1; e2 ] -> begin
        (* let n_vars = List.map make_temp_var ty in *)
        let n_cond =
          match t2ne cond with [ c ] -> c | _ -> error_ "expected single value for if operand"
        in
        let handle_e ne1 ne2 typ =
          let n_id, _, _ = make_temp_var typ in
          equs_ref := NEQ_ce (n_id, make_ncexpr (NCE_ite (n_cond, ne1, ne2)) typ) :: !equs_ref;
          make_nexpr (NE_ident n_id) typ
        in
        map3 handle_e (t2nce e1) (t2nce e2) ty
      end
    | _ -> error_ "invalid arity for ITE (expected 3)"
  in

  match expr.texpr_desc with
  | TE_const c -> [ make_1_nexpr (NE_const c) ]
  | TE_ident id -> [ make_1_nexpr (NE_ident id) ]
  | TE_op (op, le) -> begin
      (* we must split between unop, binop, ITE *)
      (* for each element, we must reduce the tuples, and duplicate the operation: (1, 2) + (3, 4) -> [1+3, 2+4]*)
      let handler =
        begin match op with
        | Op_eq -> op2 Binop_eq
        | Op_neq -> op2 Binop_neq
        | Op_lt -> op2 Binop_lt
        | Op_le -> op2 Binop_le
        | Op_gt -> op2 Binop_gt
        | Op_ge -> op2 Binop_ge
        | Op_add -> op2 Binop_add
        | Op_sub -> if List.length le = 1 then op1 Unop_neg else op2 Binop_sub
        | Op_mul -> op2 Binop_mul
        | Op_div -> op2 Binop_div
        | Op_mod -> op2 Binop_mod
        | Op_add_f -> op2 Binop_add_f
        | Op_sub_f -> op2 Binop_sub_f
        | Op_mul_f -> op2 Binop_mul_f
        | Op_div_f -> op2 Binop_div_f
        | Op_not -> op1 Unop_not
        | Op_and -> op2 Binop_and
        | Op_or -> op2 Binop_or
        | Op_impl -> op2 Binop_impl
        | Op_if -> op_if
        end
      in
      handler le expr.texpr_type
    end
  | TE_app (f, el) -> begin
      (* we need to add a new equation (temp1, ..., temp_k) = f(el) and then use it *)
      let n_vars = List.map make_temp_var expr.texpr_type in
      (* we need to do this in 2 steps, otherwise the new equations recursively added get overwritten! nasty bug :( *)
      let new_eq = NEQ_app (var_list_to_patt n_vars, f, List.map (fun e -> unpack (t2ne e)) el) in
      equs_ref := new_eq :: !equs_ref;
      List.map (fun (id, ty, _) -> make_nexpr (NE_ident id) ty) n_vars
    end
  | TE_prim (f, el) -> begin
      (* we need to add a new equation (temp1, ..., temp_k) = f(el) and then use it *)
      let n_vars = List.map make_temp_var expr.texpr_type in
      equs_ref :=
        NEQ_prim (var_list_to_patt n_vars, f, List.map (fun e -> unpack (t2ne e)) el) :: !equs_ref;
      List.map (fun (id, ty, _) -> make_nexpr (NE_ident id) ty) n_vars
    end
  | TE_arrow (e1, e2) ->
      (* x -> y is equivalent to if (true fby false) then x else y ; we add a new ident temp for true fby false *)
      (* since we must return an expr, we also add expressions temp1 = if temp then e1[0] else e2[0], etc *)
      (* TODO: we should reuse true fby false *)
      (* TODO: this could be better handled as a cexpr *)
      let n_TfbyF, _, _ = make_temp_var Tbool in
      equs_ref :=
        NEQ_fby (n_TfbyF, Cbool true, make_nexpr (NE_const (Cbool false)) Tbool) :: !equs_ref;
      let handle_v ne1 ne2 ty =
        let n_var, _, _ = make_temp_var ty in
        equs_ref :=
          NEQ_ce (n_var, make_ncexpr (NCE_ite (make_nexpr (NE_ident n_TfbyF) Tbool, ne1, ne2)) ty)
          :: !equs_ref;
        make_nexpr (NE_ident n_var) ty
      in
      map3 handle_v (t2nce e1) (t2nce e2) expr.texpr_type
  | TE_pre e ->
      (* we add temp variables and equations temp1 = pre e1, ... *)
      let handle_v ne ty =
        let n_var, _, _ = make_temp_var ty in
        Format.printf "adding a new neq_pre with var %a\n" Ident.print n_var;
        equs_ref := NEQ_pre (n_var, ne) :: !equs_ref;
        make_nexpr (NE_ident n_var) ty
      in
      List.map2 handle_v (t2ne e) expr.texpr_type
  | TE_tuple le ->
      (* we cannot have nested tuples, so each element is already reduced *)
      List.map (fun e -> unpack (t2ne e)) le
  | TE_when (e, b, ck) ->
      List.map2 (fun ne t -> make_nexpr (NE_when (ne, b, ck)) t) (t2ne e) expr.texpr_type
  | TE_merge (ck, e1, e2) ->
      (* we add temp variables and equations temp1 = merge ... *)
      let handle_v ne1 ne2 ty =
        let n_var, _, _ = make_temp_var ty in
        equs_ref := NEQ_ce (n_var, make_ncexpr (NCE_merge (ck, ne1, ne2)) ty) :: !equs_ref;
        make_nexpr (NE_ident n_var) ty
      in
      map3 handle_v (t2nce e1) (t2nce e2) expr.texpr_type
  | TE_fby (e1, e2) ->
      (* we transform it into if true -> false then e1 else dummy fby e2 *)
      (* TODO: reuse true fby false *)
      (* TODO: handle directly const fby e *)
      (* TODO: this could be better handled as a ncexpr *)
      let n_TfbyF, _, _ = make_temp_var Tbool in
      equs_ref :=
        NEQ_fby (n_TfbyF, Cbool true, make_nexpr (NE_const (Cbool false)) Tbool) :: !equs_ref;
      let handle_v nce1 ne2 ty =
        (* e1 fby e2: temp1 = dummy fby e2; temp2 = if (true -> false) then e1 else temp1; temp2*)
        let n_var_fby, _, _ = make_temp_var ty and n_var_ite, _, _ = make_temp_var ty in
        equs_ref :=
          NEQ_fby (n_var_fby, make_dummy_const ty, ne2)
          :: NEQ_ce
               ( n_var_ite,
                 make_ncexpr
                   (NCE_ite
                      ( make_nexpr (NE_ident n_TfbyF) Tbool,
                        nce1,
                        make_ncexpr (NCE_expr (make_nexpr (NE_ident n_var_fby) ty)) ty ))
                   ty )
          :: !equs_ref;
        make_nexpr (NE_ident n_var_ite) ty
      in
      map3 handle_v (t2nce e1) (t2ne e2) expr.texpr_type

and texpr_to_ncexpr (locals_ref : typed_var list ref) (equs_ref : n_eq list ref) expr : n_cexpr list
    =
  (* from a given expression of type (ty_1, ..., ty_k) return a list of k n_cexpr, each of type ty_i *)
  let t2nce = texpr_to_ncexpr locals_ref equs_ref in
  let t2ne = texpr_to_nexpr locals_ref equs_ref in
  let make_expr n_ce t = { ncexpr_desc = n_ce; ncexpr_type = t; ncexpr_loc = expr.texpr_loc } in
  let error_ = error expr.texpr_loc in

  match expr.texpr_desc with
  | TE_merge (ck, e1, e2) ->
      map3
        (fun ne1 ne2 t -> make_expr (NCE_merge (ck, ne1, ne2)) t)
        (t2nce e1) (t2nce e2) expr.texpr_type
  | TE_op (Op_if, el) ->
      begin match el with
      | [ cond; e1; e2 ] ->
          begin match t2ne cond with
          | [ cond_ne ] ->
              map3
                (fun ne1 ne2 t -> make_expr (NCE_ite (cond_ne, ne1, ne2)) t)
                (t2nce e1) (t2nce e2) expr.texpr_type
          | _ -> error_ "invalid type of if condition"
          end
      | _ -> error_ "incoherent types of expression between then and else"
      end
  (* TODO: handle more cexprs directly (merge and ite whenever possible, as well as e.g. arrow and pre) *)
  | _ -> List.map2 (fun e t -> make_expr (NCE_expr e) t) (t2ne expr) expr.texpr_type

let norm_eq (locals_ref : typed_var list ref) (equs_ref : n_eq list ref) (t_eq : t_equation) : unit
    =
  (* TODO: too much duplication! *)
  let nces = texpr_to_ncexpr locals_ref equs_ref t_eq.teq_expr in
  if List.length nces <> List.length t_eq.teq_patt.tpatt_desc then
    error t_eq.teq_expr.texpr_loc
      "incoherent length between number of variables and number of resulting ncexprs";

  let handle var ty nce =
    if ty <> nce.ncexpr_type then
      error t_eq.teq_expr.texpr_loc "type mismatch between variable and resulting ncexpr";
    equs_ref := NEQ_ce (var, nce) :: !equs_ref
  in
  iter3 handle t_eq.teq_patt.tpatt_desc t_eq.teq_patt.tpatt_type nces

let norm_node n =
  let locals_ref = ref n.tn_local in
  let equs_ref = ref [] in
  List.iter (norm_eq locals_ref equs_ref) n.tn_equs;
  {
    nn_name = n.tn_name;
    nn_input = n.tn_input;
    nn_output = n.tn_output;
    nn_local = !locals_ref;
    nn_equs = !equs_ref;
    nn_loc = n.tn_loc;
  }

let norm_file ft = List.map norm_node ft
