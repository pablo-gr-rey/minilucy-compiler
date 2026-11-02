(* Programme principal *)

open Format
open Lexing
open Lexer

let usage = "usage: " ^ Sys.argv.(0) ^ " [options] file.lus main"
let parse_only = ref false
let type_only = ref false
let norm_only = ref false
let simpl_only = ref false
let sched_only = ref false
let objc_only = ref false
let verbose = ref false
let c_folder = ref ""

let spec =
  [
    ("-parse-only", Arg.Set parse_only, "  stops after parsing");
    ("-type-only", Arg.Set type_only, "  stops after typing");
    ("-norm-only", Arg.Set norm_only, "  stops after normalization");
    ("-simpl-only", Arg.Set simpl_only, "  stops after simplifying dataflow");
    ("-sched-only", Arg.Set sched_only, "  stops after scheduling");
    ("-objc-only", Arg.Set objc_only, "  stops after conversion to Obj-C");
    ("-verbose", Arg.Set verbose, "print intermediate transformations");
    ("-v", Arg.Set verbose, "print intermediate transformations");
    ("-print-id", Arg.Set Ident.print_id, "print variables ID");
    ("-output-folder", Arg.Set_string c_folder, "folder where .c and .h files are written");
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

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)

let write_outputs basename output_dir h_content c_content =
  let h_path = Filename.concat output_dir (basename ^ ".h") in
  let c_path = Filename.concat output_dir (basename ^ ".c") in

  write_file h_path h_content;
  write_file c_path c_content

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
      Format.printf "/* Simplified norm ast                */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std simpl_nt
    end;
    if !simpl_only then exit 0;

    let sched_nt = List.map Schedule_full.schedule simpl_nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Scheduled norm ast                 */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std sched_nt
    end;
    if !sched_only then exit 0;

    let objc_nt = List.map Objc.translate_node sched_nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Obj-C                              */@.";
      Format.printf "/**************************************/@.";
      Objc_ast_printer.print_def_list_std objc_nt
    end;
    if !objc_only then exit 0;

    let basename = Filename.remove_extension (Filename.basename file) in
    let h_file, c_file = C_printer.print_c_files objc_nt basename in
    write_outputs basename !c_folder h_file c_file;
    (* TODO :o *)
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
  | Schedule_full.Error (l, e) ->
      report_loc l;
      eprintf "%a\n@." Schedule_full.report e;
      exit 1
  | Objc.Error (l, e) ->
      report_loc l;
      eprintf "%a\n@." Objc.report e;
      exit 1
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
