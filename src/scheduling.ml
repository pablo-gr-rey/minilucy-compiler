(* I changed this module to work on normalized lustre (because normalization might change the order of the equations) *)
(* I also re-implemented the topological order, the one given was in O(n^2) *)

open Asttypes
open Typed_ast
open Norm_ast

type error = ScheduleError of string

exception Error of location * error
exception Causality

let error loc fmt = Format.kasprintf (fun s -> raise (Error (loc, ScheduleError s))) fmt
let report fmt = function ScheduleError s -> Format.fprintf fmt "%s" s

module S = Set.Make (Ident)
module ISet = Set.Make (Int)
module VMap = Map.Make (Ident)

module Graph_o = Set.Make (struct
  type t = Ident.t * S.t * t_equation

  let compare (x1, s1, _) (x2, s2, _) =
    let c = Ident.compare x1 x2 in
    if c <> 0 then c else S.compare s1 s2
end)

(** [add_vars_of_patt s patt] ajoute à l'ensemble [s] les variables introduites par le motif [patt].
*)
let add_vars_of_patt s { tpatt_desc = p; _ } = List.fold_left (fun s x -> S.add x s) s p

(** [add_vars_of_exp s exp] ajoute à l'ensemble [s] les variables dont l'expression [exp] dépend
    instantanément. *)
let rec add_vars_of_exp s { texpr_desc = e; _ } =
  match e with
  | TE_const _ -> s
  | TE_ident x -> S.add x s
  | TE_arrow (e1, e2) -> add_vars_of_exp (add_vars_of_exp s e1) e2
  | TE_pre _ -> s
  | TE_op (_, el) -> List.fold_left (fun s e -> add_vars_of_exp s e) s el
  | TE_app (_, l) -> List.fold_left add_vars_of_exp s l
  | TE_prim (_, l) -> List.fold_left add_vars_of_exp s l
  | TE_tuple l -> List.fold_left add_vars_of_exp s l
  | TE_when (e, _, x) -> add_vars_of_exp (S.add x s) e
  | TE_merge (x, e1, e2) -> add_vars_of_exp (add_vars_of_exp (S.add x s) e1) e2
  | TE_fby (e1, _) -> add_vars_of_exp s e1

(* the following functions compute a map associating to a variable x the set of expressions (indices) reading this variable *)
(* they take as argument the index of the current equation and add it to the map whenever necessary *)

let add_read eq x reads =
  let update = function None -> Some (ISet.singleton eq) | Some rs -> Some (ISet.add eq rs) in
  VMap.update x update reads

let rec compute_read_ne eq reads { nexpr_desc = ne; _ } =
  match ne with
  | NE_const _ -> reads
  | NE_ident x -> add_read eq x reads
  | NE_unop (_, ne') -> compute_read_ne eq reads ne'
  | NE_binop (_, ne1, ne2) -> compute_read_ne eq (compute_read_ne eq reads ne1) ne2
  | NE_when (ne', _, x) -> add_read eq x (compute_read_ne eq reads ne')

let rec compute_read_nce eq reads { ncexpr_desc = nce; _ } =
  match nce with
  | NCE_expr ne -> compute_read_ne eq reads ne
  | NCE_merge (x, nce1, nce2) ->
      add_read eq x (compute_read_nce eq (compute_read_nce eq reads nce1) nce2)

(* from a given equation, this returns the list of variables read by the equation *)
let compute_write_read_neq eq reads = function
  | NEQ_ce (_, nce) -> compute_read_nce eq reads nce
  | NEQ_fby _ ->
      reads
      (* we only consider instantaneous dependencies, and after norm left-side is a constant *)
  | NEQ_app (_, _, nel) | NEQ_prim (_, _, nel) -> List.fold_left (compute_read_ne eq) reads nel

let schedule_equs loc equs_l =
  (* step 0: assing an index to each equation *)
  (* since we use equations as nodes of a graph structure, it is more convenient to store them as integers and use an int-indexed graph *)
  let equs = Array.of_list equs_l in
  let n_equs = Array.length equs in

  (* step 1: for each var x, build the list of expression reading the value x *)
  let reads =
    List.fold_left
      (fun reads (iEq, eq) -> compute_write_read_neq iEq reads eq)
      VMap.empty
      (List.mapi (fun iEq eq -> (iEq, eq)) equs_l)
  in

  (* step 2: construct the graph *)
  (* we add an edge from i to j if equation j must be scheduled AFTER equation i *)
  (* i.e. for all variables written by eq. i, add an edge from i to all equations reading it *)
  let graph = Graph.create n_equs in

  (* add an edge from i to j for all equations j which read the value of x *)
  let add_edges i x =
    match VMap.find_opt x reads with
    | None -> ()
    | Some s -> ISet.iter (fun j -> Graph.add_edge i j graph) s
  in

  let handle_eq i = function
    | NEQ_ce (x, _) | NEQ_fby (x, _, _) -> add_edges i x
    | NEQ_app (patt, _, _) | NEQ_prim (patt, _, _) -> List.iter (add_edges i) patt.tpatt_desc
  in
  Array.iteri handle_eq equs;

  (* step 3: construct a topological sort *)
  let topo =
    match Graph.topo graph with
    | Error n ->
        error loc "failed to schedule! found a depencency cycle including equation %a"
          Norm_ast_printer.print_neq equs.(n)
    | Topo l -> l
  in

  (* step 4: rebuild the original graph *)
  List.map (fun i -> equs.(i)) topo

let schedule_equs_old inputs equs =
  (* Construction du graphe de dépendance entre les variables. *)
  let g =
    List.fold_left
      (fun g eq ->
        let vp = add_vars_of_patt S.empty eq.teq_patt in
        let ve = add_vars_of_exp S.empty eq.teq_expr in
        S.fold (fun x g -> Graph_o.add (x, ve, eq) g) vp g)
      Graph_o.empty equs
  in
  (* Suppression des dépendances aux entrées. *)
  let g =
    let s_inputs = List.fold_left (fun acc (x, _, _) -> S.add x acc) S.empty inputs in
    Graph_o.fold (fun (y, s, e) g -> Graph_o.add (y, S.diff s s_inputs, e) g) g Graph_o.empty
  in
  (* Tri topologique des equations *)

  let rec exists_loop topo g =
    if Graph_o.is_empty g then List.rev topo
    else
      let g1, g2 = Graph_o.partition (fun (_, s, _) -> S.is_empty s) g in
      if Graph_o.is_empty g1 then raise Causality;
      let sv = Graph_o.fold (fun (x, _, _) s -> S.add x s) g1 S.empty in
      let g =
        Graph_o.fold (fun (y, s, e) g -> Graph_o.add (y, S.diff s sv, e) g) g2 Graph_o.empty
      in
      let topo = Graph_o.fold (fun (_, _, e) l -> if List.mem e l then l else e :: l) g1 topo in
      exists_loop topo g
  in
  exists_loop [] g

let schedule_node n =
  let equs = schedule_equs n.nn_loc n.nn_equs in
  { n with nn_equs = equs }

let schedule_file = List.map schedule_node
