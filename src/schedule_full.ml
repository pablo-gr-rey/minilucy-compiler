open Asttypes
open Typed_ast
open Norm_ast

type error = ScheduleError of string

exception Error of location * error

let error loc fmt = Format.kasprintf (fun s -> raise (Error (loc, ScheduleError s))) fmt
let report fmt = function ScheduleError s -> Format.fprintf fmt "%s" s

type node_status = Unseen | Current | Seen

module VMap = Map.Make (struct
  type t = Ident.t

  let compare (v1 : Ident.t) (v2 : Ident.t) = compare v1.id v2.id
end)

module ISet = Set.Make (Int)

let make_temp_var (locals : typed_var list ref) name ty ck : typed_var =
  let n_var = (Ident.make name Ident.Stream, ty, ck) in
  locals := n_var :: !locals;
  n_var

(* duplicate fbys: we replace x = a fby v by x = x' and x' = a fby v to make scheduling possible *)
(* otherwise, for example, x = 0 fby y and y = 0 fby x is well-defined but not schedulable *)
let duplicate_fby locals eqs =
  let aux eq_list = function
    | NEQ_fby (x, c, ne) ->
        (* that's a lot of words just to say x = x'... *)
        let ty = ne.nexpr_type and ck = ne.nexpr_clock and loc = ne.nexpr_loc in
        let nx, _, _ = make_temp_var locals (x.name ^ "_tmp") ty ck in
        let nx_ne =
          { nexpr_desc = NE_ident nx; nexpr_type = ty; nexpr_loc = loc; nexpr_clock = ck }
        in
        let nx_nce =
          { ncexpr_desc = NCE_expr nx_ne; ncexpr_type = ty; ncexpr_loc = loc; ncexpr_clock = ck }
        in
        NEQ_ce (x, nx_nce) :: NEQ_fby (nx, c, ne) :: eq_list
    | eq -> eq :: eq_list
  in
  List.fold_left aux [] eqs

(* call f x for each x which the expression reads *)
let rec iter_reads_ck f = function
  | TC_base -> ()
  | TC_when (ck, v) | TC_whennot (ck, v) ->
      f v;
      iter_reads_ck f ck

let rec iter_reads_ned f = function
  | NE_const _ -> ()
  | NE_ident x -> f x
  | NE_unop (_, ne) -> iter_reads_ne f ne
  | NE_binop (_, ne1, ne2) ->
      iter_reads_ne f ne1;
      iter_reads_ne f ne2
  | NE_when (ne, _, x) ->
      f x;
      iter_reads_ne f ne

and iter_reads_ne f ne =
  iter_reads_ck f ne.nexpr_clock;
  iter_reads_ned f ne.nexpr_desc

and iter_reads_nced f = function
  | NCE_expr ne -> iter_reads_ne f ne
  | NCE_merge (x, nce1, nce2) ->
      f x;
      iter_reads_nce f nce1;
      iter_reads_nce f nce2

and iter_reads_nce f nce =
  iter_reads_ck f nce.ncexpr_clock;
  iter_reads_nced f nce.ncexpr_desc

and iter_reads_eq f = function
  | NEQ_ce (_, nce) -> iter_reads_nce f nce
  | NEQ_fby (_, _, ne) -> iter_reads_ne f ne
  | NEQ_app (_, _, nes) | NEQ_prim (_, _, nes) -> List.iter (iter_reads_ne f) nes

let build_reads equs =
  (* returns a map from variable to sets of int (representing equations), where reads(x) is the set of equations which read the value of x *)
  let reads = ref VMap.empty in

  let handle_eq i eq =
    let update = function
      | None -> Some (ISet.singleton i)
      | Some s -> if ISet.mem i s then Some s else Some (ISet.add i s)
    in
    iter_reads_eq (fun x -> reads := VMap.update x update !reads) eq
  in
  Array.iteri handle_eq equs;
  !reads

let schedule_equs loc locals equs_l =
  (* step 0: duplicate fby introducing temp variables *)
  (* since we will do a quite heavy use of equations as graph structure, it is more convenient to store them as integers and use an int-indexed graph *)
  let equs = Array.of_list (duplicate_fby locals equs_l) in
  let n_equs = Array.length equs in

  (* step 1: for each var x, build the list of expression reading the value x *)
  let (reads : ISet.t VMap.t) = build_reads equs in

  (* step 2: for each equation x, construct the graph *)
  (* preds maps an equation i to the list of equations which should be scheduled BEFORE i *)
  (* this means that we need a topological order of the transposed graph (but to compute the topological order we have to transpose it, so we do it directly here) *)
  let preds = Array.make n_equs [] in
  let add_edge i j = preds.(i) <- j :: preds.(i) in

  (* apply f i for all equations i which read the value of x *)
  let iter_reads f x = match VMap.find_opt x reads with None -> () | Some s -> ISet.iter f s in

  (* we need edges x = v fby a -> reads(x) and edges reads(x) -> x+ = e+ *)
  let handle_eq i = function
    | NEQ_ce (x, _) ->
        (* x = e: we must add edges from reads(x) to eq *)
        iter_reads (fun j -> add_edge j i) x
    | NEQ_fby (x, _, _) ->
        (* x = v fby a: we must add edges from eq to reads(x) *)
        iter_reads (fun j -> add_edge i j) x
    | NEQ_app (patt, _, _) | NEQ_prim (patt, _, _) ->
        (* x+ = f(e+): we must add edges from all reads(x_i) to eq *)
        List.iter (fun x -> iter_reads (fun j -> add_edge j i) x) patt.tpatt_desc
  in
  Array.iteri handle_eq equs;

  (* step 3: construct a topological sort *)
  let status = Array.make n_equs Unseen in
  let rec dfs topo x =
    match status.(x) with
    | Unseen ->
        status.(x) <- Current;
        let n_topo = x :: List.fold_left dfs topo preds.(x) in
        status.(x) <- Seen;
        n_topo
    | Current ->
        error loc "failed to schedule! found a dependency cycle including equation %a"
          Norm_ast_printer.print_neq equs.(x)
    | Seen -> topo
  in
  let rec start_dfs topo x = if x = n_equs then topo else start_dfs (dfs topo x) (x + 1) in
  let topo = List.rev (start_dfs [] 0) in

  (* step 4: rebuild the original graph *)
  List.map (fun i -> equs.(i)) topo

let schedule node =
  let locals = ref node.nn_local in
  let equs = schedule_equs node.nn_loc locals node.nn_equs in
  { node with nn_equs = equs; nn_local = !locals }
