type t = { id : int; name : string; kind : kind }
and kind = Stream | Node | Prim

let print_id = ref false

let make, make_temp =
  let cpt = ref 0 in
  ( (fun s kind ->
      incr cpt;
      { id = !cpt; name = s; kind }),
    fun kind ->
      incr cpt;
      { id = !cpt; name = "temp" ^ string_of_int !cpt; kind } )

(* let compare = Pervasives.compare *)
let compare = Stdlib.compare (* changed for compatibility *)

let print fmt x =
  if !print_id then Format.fprintf fmt "%s__%i" x.name x.id else Format.fprintf fmt "%s" x.name

(* Utils *)
let print_to_string print x =
  ignore (Format.flush_str_formatter ());
  print Format.str_formatter x;
  Format.fprintf Format.str_formatter "@?";
  Format.flush_str_formatter ()

let string_of x = print_to_string print x
