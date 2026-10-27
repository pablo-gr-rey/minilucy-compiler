(* Programme principal *)

open Format
open Lexing
open Lexer

let usage = "usage: " ^ Sys.argv.(0) ^ " [options] file.lus main"
let parse_only = ref false
let type_only = ref false
let norm_only = ref false
let simpl_only = ref false
let lucy_printer = ref false
let ocaml_printer = ref true
let verbose = ref false

let spec =
  [
    ("-parse-only", Arg.Set parse_only, "  stops after parsing");
    ("-type-only", Arg.Set type_only, "  stops after typing");
    ("-norm-only", Arg.Set norm_only, "  stops after normalization");
    ("-simpl-only", Arg.Set simpl_only, "  stops after simplifying dataflow");
    ("-verbose", Arg.Set verbose, "print intermediate transformations");
    ("-v", Arg.Set verbose, "print intermediate transformations");
    ("-print-id", Arg.Set Ident.print_id, "print variables ID");
  ]

let file, main_node =
  let file = ref None in
  let main = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".lus") then raise (Arg.Bad "no .lus extension");
    file := Some s
  in
  let set_main s = main := Some s in
  let cpt = ref 0 in
  let set s =
    incr cpt;
    match !cpt with 1 -> set_file s | 2 -> set_main s | _ -> raise (Arg.Bad "Too many arguments")
  in
  Arg.parse spec set usage;
  ( (match !file with
    | Some f -> f
    | None ->
        Arg.usage spec usage;
        exit 1),
    match !main with
    | Some n -> n
    | None ->
        Arg.usage spec usage;
        exit 1 )

let report_loc (b, e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  Format.set_margin 100;
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;

    let ft = Typing.type_file f in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Typed ast                          */@.";
      Format.printf "/**************************************/@.";
      Typed_ast_printer.print_node_list_std ft
    end;
    if !type_only then exit 0;

    let nt = Norm.norm_file ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Norm ast                           */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std nt
    end;
    if !norm_only then exit 0;

    let simpl_nt = List.map Simpl_dataflow.simpl_dataflow nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Simplified Norm ast                */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std simpl_nt
    end;

    (* XXX TODO XXX *)
    Format.printf "Don't know@.";

    exit 0
  with
  | Lexical_error s ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
  | Parsing.Parse_error ->
      report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error\n@.";
      exit 1
  | Typing.Error (l, e) ->
      report_loc l;
      eprintf "%a\n@." Typing.report e;
      exit 1
  | Norm.Error (l, e) ->
      report_loc l;
      eprintf "%a\n@." Norm.report e;
      exit 1
  | Simpl_dataflow.Error (l, e) ->
      report_loc l;
      eprintf "%a\n@." Simpl_dataflow.report e;
      exit 1
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
