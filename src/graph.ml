(* implements a simple oriented graph structure (where nodes are integers from 0 to N-1), implements topological order. uses Set as the internal adjacency list types, so it's fine to add several times the same edge *)
(* most modifications are in-place! *)

module ISet = Set.Make (Int)

type t = { n : int; adj : ISet.t array }

(* for topological order *)
type status = Unseen | Current | Seen
type topo_res = Error of int | Topo of int list

let create n = { n; adj = Array.make n ISet.empty }

(* does not check that i and j are between 0 and N-1 *)
let add_edge i j g = g.adj.(i) <- ISet.add j g.adj.(i)

let transpose g =
  let ng = create g.n in
  let rec handle_node i =
    if i < g.n then begin
      ISet.iter (fun j -> add_edge j i ng) g.adj.(i);
      handle_node (i + 1)
    end
  in
  handle_node 0;
  ng

(* returns a topological order of the graph g, or None if it is not acyclic *)
let topo g =
  let n = g.n in
  let g_trans = transpose g in
  (* unseen = not seen; current = being computed; seen = over *)
  (* if we go back to a current node during the dfs, it means there is a cycle *)
  let status = Array.make n Unseen in

  let rec dfs x = function
    | Error n -> Error n
    | Topo topo ->
        begin match status.(x) with
        | Unseen ->
            status.(x) <- Current;
            let n_topo = ISet.fold dfs g_trans.adj.(x) (Topo topo) in
            status.(x) <- Seen;
            begin match n_topo with Error n -> Error n | Topo n_topo_l -> Topo (x :: n_topo_l)
            end
        | Current -> Error x (* cyclic graph *)
        | Seen -> Topo topo
        end
  in

  (* now we start the dfs from each node *)
  let rec start_dfs x topo = if x = n then topo else start_dfs (x + 1) (dfs x topo) in

  match start_dfs 0 (Topo []) with Error n -> Error n | Topo l -> Topo (List.rev l)
