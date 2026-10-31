open Asttypes
open Typed_ast
open Norm
open Objc_ast
open Norm_ast

type error = ObjcError of string

exception Error of location * error

let error loc fmt = Format.kasprintf (fun s -> raise (Error (loc, ObjcError s))) fmt
let report fmt = function ObjcError s -> Format.fprintf fmt "%s" s
let tvar_to_ocvar ((id, ty, _) : typed_var) : oc_var = (id, ty)

let rec control ck instr =
  match ck with
  | TC_base -> instr
  | TC_when (ck, v) -> control ck (OCS_case (v, [ instr ], []))
  | TC_whennot (ck, v) -> control ck (OCS_case (v, [], [ instr ]))

let rec translate_expr ctx vars ne =
  match ne.nexpr_desc with
  | NE_const c -> OCE_const c
  | NE_ident x ->
      if List.exists (fun (var, _) -> x = var) ctx.occ_mem then OCE_state x
      else begin
        if not (VMap.mem x vars) then error ne.nexpr_loc "unknown variable %a" Ident.print x;
        OCE_var x
      end
  | NE_unop (u, ne) ->
      let oce = translate_expr ctx vars ne in
      OCE_unop (u, oce)
  | NE_binop (b, ne1, ne2) ->
      let oce1 = translate_expr ctx vars ne1 and oce2 = translate_expr ctx vars ne2 in
      OCE_binop (b, oce1, oce2)
  | NE_when (ne, _, _) -> translate_expr ctx vars ne
(* here, we drop the when and the variable because the clock is already taken into account (the whole expression will end up in a case) *)

let rec translate_assign ctx vars x nce =
  match nce.ncexpr_desc with
  | NCE_expr ne -> OCS_assign (x, translate_expr ctx vars ne)
  | NCE_merge (var, nceT, nceF) ->
      OCS_case (var, [ translate_assign ctx vars x nceT ], [ translate_assign ctx vars x nceF ])

let translate_eq vars eq ctx =
  match eq with
  | NEQ_ce (x, nce) ->
      {
        ctx with
        occ_instrs = control nce.ncexpr_clock (translate_assign ctx vars x nce) :: ctx.occ_instrs;
      }
  | NEQ_fby (x, c, ne) ->
      let x_var, ck, typ = VMap.find x vars in
      if typ = V_input then
        error ne.nexpr_loc
          "variable %a assigned to a fby expression should be a local variable, but got an input"
          Ident.print x;
      if typ = V_output then (* TODO? output variables could be left hand side of fbys *)
        error ne.nexpr_loc
          "variable %a assigned to a fby expression should be a local variable, but got an output"
          Ident.print x;
      {
        ctx with
        occ_mem = x_var :: ctx.occ_mem;
        occ_init = OCS_state_assign (x, OCE_const c) :: ctx.occ_init;
        occ_vars = List.filter (fun (var, _) -> x <> var) ctx.occ_vars;
        occ_instrs = control ck (OCS_state_assign (x, translate_expr ctx vars ne)) :: ctx.occ_instrs;
      }
  | NEQ_app (pat, f, nel) ->
      let ocel = List.map (translate_expr ctx vars) nel in
      let o = Ident.make_w_id f.name Object in
      let ck = List.hd pat.tpatt_clock in
      (* in our simplified model, all input/output are on the same clock *)
      {
        ctx with
        occ_init = OCS_reset o :: ctx.occ_init;
        occ_insts = (o, f) :: ctx.occ_insts;
        occ_instrs = control ck (OCS_step (pat.tpatt_desc, o, ocel)) :: ctx.occ_instrs;
      }
  | NEQ_prim (pat, f, nel) ->
      let ocel = List.map (translate_expr ctx vars) nel in
      let ck = List.hd pat.tpatt_clock in
      { ctx with occ_instrs = control ck (OCS_prim (pat.tpatt_desc, f, ocel)) :: ctx.occ_instrs }

let rec join_stmt = function
  | [] -> []
  | [ s ] -> [ s ]
  | OCS_case (x1, s1T, s1F) :: OCS_case (x2, s2T, s2F) :: l when x1 = x2 ->
      let merged = OCS_case (x1, join_stmt (s1T @ s2T), join_stmt (s1F @ s2F)) in
      join_stmt (merged :: l)
  | s :: l -> s :: join_stmt l

let translate_node node =
  let init_ctx =
    {
      occ_mem = [];
      occ_init = [];
      occ_insts = [];
      occ_vars = List.map tvar_to_ocvar node.nn_local;
      occ_instrs = [];
    }
  in

  let add_var kind vars (v : typed_var) =
    let id, _, ck = v in
    let ocv = tvar_to_ocvar v in
    VMap.add id (ocv, ck, kind) vars
  in
  let vars = List.fold_left (add_var V_input) VMap.empty node.nn_input in
  let vars = List.fold_left (add_var V_output) vars node.nn_output in
  let vars = List.fold_left (add_var V_local) vars node.nn_local in

  (* we must fold from end to beginning!! this way, state variables are necessarily written to (with fby) before they get read, and they appear correctly in the context *)
  let ctx = List.fold_right (translate_eq vars) node.nn_equs init_ctx in

  {
    ocd_name = node.nn_name;
    ocd_mem = ctx.occ_mem;
    ocd_insts = ctx.occ_insts;
    ocd_reset = ctx.occ_init;
    ocd_step =
      ( List.map tvar_to_ocvar node.nn_input,
        List.map tvar_to_ocvar node.nn_output,
        ctx.occ_vars,
        join_stmt ctx.occ_instrs );
  }
