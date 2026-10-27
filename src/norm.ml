open Asttypes
open Typed_ast
open Norm_ast
open Typed_ast_printer

type error = ExpectedClock of t_clock * t_clock | Other of string

exception Error of location * error

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)
let error loc err = raise (Error (loc, err))
let error_other loc fmt = Format.kasprintf (fun s -> raise (Error (loc, Other s))) fmt

let report fmt = function
  | ExpectedClock (c1, c2) ->
      Format.fprintf fmt "this expression has clock %a but is expected to have clock %a" print_clock
        c1 print_clock c2
  | Other s -> Format.fprintf fmt "%s" s

type var_kind = V_input | V_output | V_local
type var_type = { v_var : typed_var; v_kind : var_kind }

module VMap = Map.Make (struct
  type t = Ident.t

  let compare = compare
end)

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
  | _ -> failwith "invalid iter3 call: lists have different lengths"

let rec iter4 f l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
  | [], [], [], [] -> ()
  | e1 :: l1', e2 :: l2', e3 :: l3', e4 :: l4' ->
      f e1 e2 e3 e4;
      iter4 f l1' l2' l3' l4'
  | _ -> failwith "invalid iter4 call: lists have different lengths"

let make_dummy_const = function Tbool -> Cbool false | Tint -> Cint 0 | Treal -> Creal 0.0

let unpack = function
  | [ e ] -> e
  | l -> failwith (Printf.sprintf "invalid unpack: expected 1 element, got %d" (List.length l))

(* check that all elements of the list are equal; if so, return the common element; if not (or list Empty), None *)
let check_all_eq = function
  | [] -> None
  | x :: l -> if List.for_all (fun y -> x = y) l then Some x else None

let rec texpr_to_nexpr (vars : (typed_var * var_kind) VMap.t ref) (locals_ref : typed_var list ref)
    (equs_ref : n_eq list ref) expr (exp_clocks : t_clock list) : n_expr list =
  (* from a single texpr of type [t1, ..., tk] and expected clocks exp_clocks, returns a list of texprs [e1: t1, ..., ek: tk] containing no TE_tuple, TE_app or TE_prim inside *)
  let make_nexpr nexpr_d ty ck =
    { nexpr_desc = nexpr_d; nexpr_type = ty; nexpr_loc = expr.texpr_loc; nexpr_clock = ck }
  in
  let make_ncexpr ncexpr_d ty ck =
    { ncexpr_desc = ncexpr_d; ncexpr_type = ty; ncexpr_loc = expr.texpr_loc; ncexpr_clock = ck }
  in
  let make_texpr texpr_d ty =
    { texpr_desc = texpr_d; texpr_type = ty; texpr_loc = expr.texpr_loc }
  in
  let error_ fmt = error_other expr.texpr_loc fmt in
  let error_ck ck1 ck2 = error expr.texpr_loc (ExpectedClock (ck1, ck2)) in

  let make_1_nexpr nexpr_d =
    match (expr.texpr_type, exp_clocks) with
    | [ ty ], [ ck ] -> make_nexpr nexpr_d ty ck
    | _ -> error_ "expected only 1 type"
  in

  let make_temp_var ty ck : typed_var =
    let n_var = (Ident.make_temp Ident.Stream, ty, ck) in
    let n_id, _, _ = n_var in
    locals_ref := n_var :: !locals_ref;
    vars := VMap.add n_id (n_var, V_local) !vars;
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
  let t2ne = texpr_to_nexpr vars locals_ref equs_ref in
  let t2nce = texpr_to_ncexpr vars locals_ref equs_ref in

  (* convenient functions to handle operators: op1 takes e.g. neg ([te_1, ..., te_k]) with expected clock types cks and returns [neg ne_1, ..., neg ne_k] *)
  (* op2 similarly takes e.g. add [[te_1, ..., te_k], [te_1', ..., te_k']] and returns [add ne_1 ne_1', ..., add ne_k ne_k'] *)
  (* for operators (including if), clock inference is easy: all operands should have the same clock as the return value *)
  let op1 (u : unop) (el : t_expr list) (ty : ty) (cks : t_clock list) =
    match el with
    | [ e ] -> map3 (fun ne t ck -> make_nexpr (NE_unop (u, ne)) t ck) (t2ne e cks) ty cks
    | _ -> error_ "invalid arity for unary operator (expected 1)"
  in

  let op2 (b : binop) (el : t_expr list) (ty : ty) (cks : t_clock list) =
    match el with
    | [ e1; e2 ] ->
        map4
          (fun ne1 ne2 t ck -> make_nexpr (NE_binop (b, ne1, ne2)) t ck)
          (t2ne e1 cks) (t2ne e2 cks) ty cks
    | _ -> error_ "invalid arity for binary operator (expected 2)"
  in

  let op_if (el : t_expr list) (ty : ty) (cks : t_clock list) =
    (* since we have to build an expr, we have to put the if in a new equation and introduce temp variables :( *)
    match el with
    | [ cond; e1; e2 ] -> begin
        let handle_e ne1 ne2 typ ck =
          let n_id, _, _ = make_temp_var typ ck in
          let c_var, _, _ = make_temp_var Tbool ck in
          let n_cond =
            (* we have to recompute n_cond every time because the clock might be different *)
            match t2nce cond [ ck ] with
            | [ c ] -> c
            | _ -> error_ "expected single value for if operand"
          in
          let ne1w = make_nexpr (NE_when (ne1, true, c_var)) typ (TC_when (ck, c_var)) in
          let ne2w = make_nexpr (NE_when (ne2, false, c_var)) typ (TC_whennot (ck, c_var)) in
          equs_ref :=
            NEQ_ce (c_var, n_cond)
            :: NEQ_ce
                 ( n_id,
                   make_ncexpr
                     (NCE_merge
                        ( c_var,
                          make_ncexpr (NCE_expr ne1w) typ (TC_when (ck, c_var)),
                          make_ncexpr (NCE_expr ne2w) typ (TC_whennot (ck, c_var)) ))
                     typ ck )
            :: !equs_ref;
          (* equs_ref := NEQ_ce (n_id, make_ncexpr (NCE_ite (n_cond, ne1, ne2)) typ ck) :: !equs_ref; *)
          make_nexpr (NE_ident n_id) typ ck
        in
        map4 handle_e (t2ne e1 cks) (t2ne e2 cks) ty cks
      end
    | _ -> error_ "invalid arity for ITE (expected 3)"
  in

  match expr.texpr_desc with
  | TE_const c -> [ make_1_nexpr (NE_const c) ]
  | TE_ident id ->
      if not (VMap.mem id !vars) then error_ "unknown variable";
      let (_, _, ck), _ = VMap.find id !vars in
      begin match exp_clocks with
      | [ exp_ck ] -> if ck <> exp_ck then error_ck ck exp_ck
      | _ ->
          error_ "incoherent number of clock for variable (expected 1, got %d)"
            (List.length exp_clocks)
      end;
      [ make_1_nexpr (NE_ident id) ]
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
      handler le expr.texpr_type exp_clocks
    end
  | TE_app (f, el) -> begin
      (* here, we restrict ourselves to the case where all input and output are on the same clock; otherwise, we need a more complicated typing system *)
      (* this means that all clocks in the output must be the same, and that the clock of all inputs is this one *)
      if exp_clocks = [] then error_ "empty clock list";
      let ck =
        match check_all_eq exp_clocks with
        | Some ck -> ck
        | None ->
            error_ "function call should have identical clocks as return values, got %a"
              print_clock_list exp_clocks
      in

      (* we need to add a new equation (temp1, ..., temp_k) = f(el) and then use it *)
      let n_vars = List.map (fun ty -> make_temp_var ty ck) expr.texpr_type in
      (* we need to do this in 2 steps, otherwise the new equations recursively added get overwritten! nasty bug :( *)
      let new_eq =
        NEQ_app (var_list_to_patt n_vars, f, List.map (fun e -> unpack (t2ne e [ ck ])) el)
      in
      equs_ref := new_eq :: !equs_ref;
      List.map (fun (id, ty, _) -> make_nexpr (NE_ident id) ty ck) n_vars
    end
  | TE_prim (f, el) -> begin
      (* we need to add a new equation (temp1, ..., temp_k) = f(el) and then use it *)
      if exp_clocks = [] then error_ "empty clock list";
      let ck =
        match check_all_eq exp_clocks with
        | Some ck -> ck
        | None ->
            error_ "function call should have identical clocks as return values, got %a"
              print_clock_list exp_clocks
      in

      (* we need to add a new equation (temp1, ..., temp_k) = f(el) and then use it *)
      let n_vars = List.map (fun ty -> make_temp_var ty ck) expr.texpr_type in
      (* we need to do this in 2 steps, otherwise the new equations recursively added get overwritten! nasty bug :( *)
      let new_eq =
        NEQ_prim (var_list_to_patt n_vars, f, List.map (fun e -> unpack (t2ne e [ ck ])) el)
      in
      equs_ref := new_eq :: !equs_ref;
      List.map (fun (id, ty, _) -> make_nexpr (NE_ident id) ty ck) n_vars
    end
  | TE_arrow (e1, e2) ->
      (* e1 -> e2 is equivalent to if (true fby false) then e1 else e2 ; we add a new ident temp for true fby false *)
      (* since we must return an expr, we also add expressions temp1 = if temp then e1[0] else e2[0], etc *)
      (* TODO: we should reuse true fby false *)
      (* TODO: this could be better handled as a cexpr *)
      let tfbyf =
        make_texpr
          (TE_fby
             ( make_texpr (TE_const (Cbool true)) [ Tbool ],
               make_texpr (TE_const (Cbool false)) [ Tbool ] ))
          [ Tbool ]
      in
      t2ne (make_texpr (TE_op (Op_if, [ tfbyf; e1; e2 ])) expr.texpr_type) exp_clocks
      (* let handle_v ne1 ne2 ty ck =
        let n_TfbyF, _, _ = make_temp_var Tbool ck in
        equs_ref :=
          NEQ_fby (n_TfbyF, Cbool true, make_nexpr (NE_const (Cbool false)) Tbool ck) :: !equs_ref;
        let n_var, _, _ = make_temp_var ty ck in
        equs_ref :=
          (* NEQ_ce
            (n_var, make_ncexpr (NCE_ite (make_nexpr (NE_ident n_TfbyF) Tbool ck, ne1, ne2)) ty ck) *)
          NEQ_ce
            ( n_var,
              make_ncexpr
                (NCE_merge
                   ( n_TfbyF,
                     make_ncexpr
                       (NCE_expr
                          (make_nexpr (NE_when (ne1, true, n_TfbyF)) ty (TC_when (ck, n_TfbyF))))
                       ty
                       (TC_when (ck, n_TfbyF)),
                     make_ncexpr
                       (NCE_expr
                          (make_nexpr (NE_when (ne2, false, n_TfbyF)) ty (TC_whennot (ck, n_TfbyF))))
                       ty
                       (TC_whennot (ck, n_TfbyF)) ))
                ty ck )
          :: !equs_ref;
        
        make_nexpr (NE_ident n_var) ty ck
      in
      map4 handle_v (t2ne e1 exp_clocks) (t2ne e2 exp_clocks) expr.texpr_type exp_clocks *)
  | TE_pre e ->
      (* we add temp variables and equations temp1 = pre e1, ... *)
      let handle_v ne ty ck =
        let n_var, _, _ = make_temp_var ty ck in
        equs_ref := NEQ_pre (n_var, ne) :: !equs_ref;
        make_nexpr (NE_ident n_var) ty ck
      in
      map3 handle_v (t2ne e exp_clocks) expr.texpr_type exp_clocks
  | TE_tuple le ->
      (* we cannot have nested tuples, so each element is already reduced *)
      List.map2 (fun e ck -> unpack (t2ne e [ ck ])) le exp_clocks
  | TE_when (e, b, ck_var) -> begin
      if List.length exp_clocks <> 1 then error_ "cannot type a when expression on a tuple";
      let exp_ck = unpack exp_clocks in
      (* we must have either expr = e when v, exp_ck = sck when v and e: sck and v: sck
      or expr = e when not v, exp_ck = sck when not v and e: sck*)
      let sub_ck =
        match exp_ck with
        | TC_base ->
            error_
              "incompatible clocks: expected an expression on base clock, but got a when expression"
        | TC_when _ when not b ->
            error_
              "incompatible clocks: expected an expression on clock %a, but got a when not \
               expression"
              print_clock exp_ck
        | TC_when (s_ck, var) ->
            if var = ck_var then s_ck
            else
              error_
                "incompatible clocks: expected an expression on clock %a, but got an incompatible \
                 variable %a"
                print_clock exp_ck Ident.print var
        | TC_whennot _ when b ->
            error_
              "incompatible clocks: expected an expression on clock %a, but got a when expression"
              print_clock exp_ck
        | TC_whennot (s_ck, var) ->
            if var = ck_var then s_ck
            else
              error_
                "incompatible clocks: expected an expression on clock %a, but got an incompatible \
                 variable %a"
                print_clock exp_ck Ident.print var
      in
      if not (VMap.mem ck_var !vars) then error_ "unknown when variable %a" Ident.print ck_var;
      let (_, _, ck_var_ck), _ = VMap.find ck_var !vars in
      if ck_var_ck <> sub_ck then error_ck ck_var_ck sub_ck;
      List.map2
        (fun ne t -> make_nexpr (NE_when (ne, b, ck_var)) t exp_ck)
        (t2ne e [ sub_ck ]) expr.texpr_type
    end
  | TE_merge (ck_var, e1, e2) ->
      (* we add temp variables and equations temp1 = merge ... *)
      let handle_v ne1 ne2 ty ck =
        let n_var, _, _ = make_temp_var ty ck in
        equs_ref := NEQ_ce (n_var, make_ncexpr (NCE_merge (ck_var, ne1, ne2)) ty ck) :: !equs_ref;
        make_nexpr (NE_ident n_var) ty ck
      in
      map4 handle_v
        (t2nce e1 (List.map (fun ck -> TC_when (ck, ck_var)) exp_clocks))
        (t2nce e2 (List.map (fun ck -> TC_whennot (ck, ck_var)) exp_clocks))
        expr.texpr_type exp_clocks
  | TE_fby (e1, e2) ->
      (* we transform e1 fby e2 into if true -> false then e1 else dummy fby e2 *)
      (* aka n_var_fby = dummy fby e2; n_var_merge = merge tfbyf (true -> e1 when tfbyf) (false -> n_var_fby when tfbyf); n_var_merge *)
      (* TODO: reuse true fby false *)
      (* TODO: handle directly const fby e *)
      (* TODO: this could be better handled as a ncexpr *)
      let handle_v ne1 ne2 ty ck =
        (* e1 fby e2: temp1 = dummy fby e2; temp2 = if (true -> false) then e1 else temp1; temp2*)
        let n_var_fby, _, _ = make_temp_var ty ck in
        match ne1.nexpr_desc with
        | NE_const c ->
            equs_ref := NEQ_fby (n_var_fby, c, ne2) :: !equs_ref;
            make_nexpr (NE_ident n_var_fby) ty ck
        | _ -> begin
            let n_TfbyF, _, _ = make_temp_var Tbool ck in
            equs_ref :=
              NEQ_fby (n_TfbyF, Cbool true, make_nexpr (NE_const (Cbool false)) Tbool ck)
              :: !equs_ref;
            equs_ref := NEQ_fby (n_var_fby, make_dummy_const ty, ne2) :: !equs_ref;
            let n_var_merge, _, _ = make_temp_var ty ck in
            let nce1w =
              make_ncexpr
                (NCE_expr (make_nexpr (NE_when (ne1, true, n_TfbyF)) ty (TC_when (ck, n_TfbyF))))
                ty
                (TC_when (ck, n_TfbyF))
            in
            let nce2w =
              make_ncexpr
                (NCE_expr
                   (make_nexpr
                      (NE_when (make_nexpr (NE_ident n_var_fby) ty ck, false, n_TfbyF))
                      ty
                      (TC_whennot (ck, n_TfbyF))))
                ty
                (TC_whennot (ck, n_TfbyF))
            in
            equs_ref :=
              NEQ_ce (n_var_merge, make_ncexpr (NCE_merge (n_TfbyF, nce1w, nce2w)) ty ck)
              :: !equs_ref;
            make_nexpr (NE_ident n_var_merge) ty ck
          end
      in
      map4 handle_v (t2ne e1 exp_clocks) (t2ne e2 exp_clocks) expr.texpr_type exp_clocks

and texpr_to_ncexpr (vars : (typed_var * var_kind) VMap.t ref) (locals_ref : typed_var list ref)
    (equs_ref : n_eq list ref) expr (exp_clocks : t_clock list) : n_cexpr list =
  (* from a given expression of type (ty_1, ..., ty_k) return a list of k n_cexpr, each of type ty_i *)
  let t2nce = texpr_to_ncexpr vars locals_ref equs_ref in
  let t2ne = texpr_to_nexpr vars locals_ref equs_ref in
  let make_ncexpr n_ce t ck =
    { ncexpr_desc = n_ce; ncexpr_type = t; ncexpr_loc = expr.texpr_loc; ncexpr_clock = ck }
  in

  match expr.texpr_desc with
  | TE_merge (ck_var, e1, e2) ->
      map4
        (fun ne1 ne2 t ck -> make_ncexpr (NCE_merge (ck_var, ne1, ne2)) t ck)
        (t2nce e1 (List.map (fun c -> TC_when (c, ck_var)) exp_clocks))
        (t2nce e2 (List.map (fun c -> TC_whennot (c, ck_var)) exp_clocks))
        expr.texpr_type exp_clocks
  (* | TE_op (Op_if, el) ->
      (* to clock if c then (e_1, ..., e_k) else (f_1, ..., f_k), for each i, e_i, f_i and cond_i must be valid on clock exp_i *)
      begin match el with
      | [ cond; e1; e2 ] -> begin
          let handle_e nce1 nce2 typ ck =
            let n_cond =
              (* we have to recompute n_cond every time because the clock might be different *)
              match t2ne cond [ ck ] with
              | [ c ] -> c
              | _ -> error_ "expected single value for if operand"
            in
            make_ncexpr (NCE_ite (n_cond, nce1, nce2)) typ ck
          in
          map4 handle_e (t2nce e1 exp_clocks) (t2nce e2 exp_clocks) expr.texpr_type exp_clocks
        end
      | _ -> error_other expr.texpr_loc "incoherent types of expression between then and else"
      end *)
  (* TODO: handle more cexprs directly (merge and ite whenever possible, as well as e.g. arrow and pre) *)
  | _ ->
      map3
        (fun e t ck -> make_ncexpr (NCE_expr e) t ck)
        (t2ne expr exp_clocks) expr.texpr_type exp_clocks

let norm_eq (vars : (typed_var * var_kind) VMap.t ref) (locals_ref : typed_var list ref)
    (equs_ref : n_eq list ref) (t_eq : t_equation) : unit =
  (* TODO: too much duplication! *)
  let add_eq neq = equs_ref := neq :: !equs_ref in

  (* we try to catch equations we can directly transform: fby, pre, app and prim *)
  let shortcut =
    begin match t_eq.teq_expr.texpr_desc with
    (* | TE_fby (te1, te2) ->
        (* if left-side is a const or tuple of const, great; if not, we have to create new variable *)
        begin
          let unpack_const e =
            match e.texpr_desc with TE_const c -> c | _ -> failwith "failed to unpack const"
          in
          let is_const e = match e.texpr_desc with TE_const _ -> true | _ -> false in
          let add_fby_cst var cst ne = add_eq (NEQ_fby (var, unpack_const cst, ne)) in
          let handle_consts lcst = iter3 add_fby_cst t_eq.teq_patt.tpatt_desc lcst (t2ne te2) in
          (* if left-side is a const or a tuple of const, we can directly convert into fbys *)
          (* otherwise, we have to create temp variables for the left-side and directly assign *)
          (* this is still better than using NEQ_ce with a temp variable only used for the fby *)
          match te1.texpr_desc with
          | TE_const _ ->
              handle_consts [ te1 ];
              true
          | TE_tuple el when List.for_all is_const el ->
              handle_consts el;
              true
          | _ -> false
        end *)
    | _ -> false
    end
  in

  (* if we failed to find a good form, then we have to convert the right side to ncexprs and use assigns *)
  (* TODO: if we get a NE_ident as the nces and it is only used there, we can remove it to simplify the dataflow *)
  let exp_cks = t_eq.teq_patt.tpatt_clock in

  if not shortcut then begin
    let nces = texpr_to_ncexpr vars locals_ref equs_ref t_eq.teq_expr exp_cks in
    if List.length nces <> List.length t_eq.teq_patt.tpatt_desc then
      error_other t_eq.teq_expr.texpr_loc
        "incoherent length between number of variables %d and number of resulting ncexprs %d"
        (List.length t_eq.teq_patt.tpatt_desc)
        (List.length nces);

    let handle var ty nce ck =
      if ty <> nce.ncexpr_type then
        error_other t_eq.teq_expr.texpr_loc
          "type mismatch between variable (%a) and resulting ncexpr (%a)" print_base_type ty
          print_base_type nce.ncexpr_type;
      if ck <> nce.ncexpr_clock then
        error_other t_eq.teq_expr.texpr_loc
          "clock mismatch between variable (%a) and resulting ncexpr (%a)" print_clock ck
          print_clock nce.ncexpr_clock;
      add_eq (NEQ_ce (var, nce))
    in
    iter4 handle t_eq.teq_patt.tpatt_desc t_eq.teq_patt.tpatt_type nces exp_cks
  end

let norm_node n =
  let locals_ref = ref n.tn_local in
  let equs_ref = ref [] in
  let vars = ref VMap.empty in
  let add_var kind (v : typed_var) =
    let id, _, _ = v in
    vars := VMap.add id (v, kind) !vars
  in
  List.iter (add_var V_input) n.tn_input;
  List.iter (add_var V_output) n.tn_output;
  List.iter (add_var V_local) n.tn_local;

  List.iter (norm_eq vars locals_ref equs_ref) n.tn_equs;
  {
    nn_name = n.tn_name;
    nn_input = n.tn_input;
    nn_output = n.tn_output;
    nn_local = !locals_ref;
    nn_equs = !equs_ref;
    nn_loc = n.tn_loc;
  }

let norm_file ft = List.map norm_node ft
