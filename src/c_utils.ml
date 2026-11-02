open Asttypes
open Objc_ast
open Norm

type error = CPrintError of string

exception Error of location * error

let error loc fmt = Format.kasprintf (fun s -> raise (Error (loc, CPrintError s))) fmt
let report fmt = function CPrintError s -> Format.fprintf fmt "%s" s

type struct_info = {
  struct_name : Ident.t;
  struct_fields : (Ident.t * base_ty) list;
  struct_insts : (Ident.t * struct_info) list;
}

type node_info = {
  node_step : Ident.t; (* name of step function *)
  node_reset : Ident.t option; (* name of reset function, if node has memory *)
  node_ret : struct_info; (* return struct *)
  node_mem : struct_info option; (* memory struct, if node has memory *)
  ret_name : Ident.t; (* name of the local variable used to store the result of the node *)
  var_map : var_kind VMap.t;
  node_origin : oc_definition;
}

let build_def_info (defs_map, defs_list) def =
  let has_mem = def.ocd_mem <> [] || def.ocd_insts <> [] in
  let mem_name =
    if has_mem then Some (Ident.make (def.ocd_name.name ^ "_mem") Ident.Struct) else None
  in
  let ret_name = Ident.make (def.ocd_name.name ^ "_ret") Ident.Struct in
  let step_inputs, step_outputs, step_locals, _ = def.ocd_step in
  (* since we expect node to refer to previous nodes only (which also ensures there is no mutually recursive calls without having to compute a topological order), we can assume each instances refers to a previous node, which has already been added to defs_map *)
  (* we remove instances of memoryless nodes *)
  let handle_inst (obj, nn) =
    if not (VMap.mem nn defs_map) then
      error dummy_loc "unknown instance type %a in node %a" Ident.print_full nn Ident.print_full
        def.ocd_name;
    let node_inf = VMap.find nn defs_map in
    match node_inf.node_mem with None -> None | Some mem -> Some (obj, mem)
  in

  let add_var kind vmap (var, _) = VMap.add var kind vmap in
  let vmap = List.fold_left (add_var V_input) VMap.empty step_inputs in
  let vmap = List.fold_left (add_var V_local) vmap step_locals in
  let vmap = List.fold_left (add_var V_output) vmap step_outputs in

  let n_def =
    {
      node_step = Ident.make (def.ocd_name.name ^ "_step") Ident.Function;
      node_reset =
        (if has_mem then Some (Ident.make (def.ocd_name.name ^ "_reset") Ident.Function) else None);
      node_ret = { struct_name = ret_name; struct_fields = step_outputs; struct_insts = [] };
      node_mem =
        (if has_mem then
           Some
             {
               struct_name = Option.get mem_name;
               struct_fields = def.ocd_mem;
               struct_insts = List.filter_map handle_inst def.ocd_insts;
             }
         else None);
      node_origin = def;
      ret_name = Ident.make (def.ocd_name.name ^ "_result") Ident.Object;
      var_map = vmap;
      (* we also need ret_name and var_type *)
    }
  in
  (VMap.add def.ocd_name n_def defs_map, n_def :: defs_list)
