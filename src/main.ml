(* Programme principal *)

open Format
open Lexing
open Lexer

let usage = "usage: " ^ Sys.argv.(0) ^ " [options] file.lus main"
let parse_only = ref false
let type_only = ref false
let norm_only = ref false
let sched_only = ref false
let simpl_only = ref false
let sched_fby_only = ref false
let objc_only = ref false
let verbose = ref false
let c_folder = ref ""
let gen_c_main = ref false
let compare_hept = ref false
let compile = ref false
let all_nodes = ref false
let run_main = ref false

let spec =
  [
    ("-parse-only", Arg.Set parse_only, "  stops after parsing");
    ("-type-only", Arg.Set type_only, "  stops after typing");
    ("-norm-only", Arg.Set norm_only, "  stops after normalization");
    ("-sched-only", Arg.Set sched_only, "  stops after scheduling");
    ("-simpl-only", Arg.Set simpl_only, "  stops after simplifying dataflow");
    ("-sched-only", Arg.Set sched_fby_only, "  stops after scheduling fbys");
    ("-objc-only", Arg.Set objc_only, "  stops after conversion to Obj-C");
    ("-verbose", Arg.Set verbose, "print intermediate transformations");
    ("-v", Arg.Set verbose, "print intermediate transformations");
    ("-print-id", Arg.Set Ident.print_id, "print variables ID");
    ("-output-folder", Arg.Set_string c_folder, "folder where .c and .h files are written");
    ("-gen-main", Arg.Set gen_c_main, "generate a main.c file allowing simulation of the node");
    ( "-compare-hept",
      Arg.Set compare_hept,
      "generate a main.c file comparing output of this compiler and output of hept on random input"
    );
    ("-compile", Arg.Set compile, "compile files with gcc");
    ("-all-nodes", Arg.Set all_nodes, "compare all nodes with hept (requires -compare-hept)");
    ("-run-main", Arg.Set run_main, "run the produced executable");
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
    main )
(* match !main with
    | Some n -> n
    | None ->
        Arg.usage spec usage;
        exit 1 ) *)

let write_file path content =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)

let write_outputs basename output_dir h_content c_content main_content =
  Hept_utils.run_command "mkdir" [ "-p"; output_dir ];

  let h_path = Filename.concat output_dir (basename ^ ".h") in
  let c_path = Filename.concat output_dir (basename ^ ".c") in

  write_file h_path h_content;
  write_file c_path c_content;

  if Option.is_some main_content then begin
    let name = if basename = "main" then "main_" else "main" in
    let main_path = Filename.concat output_dir (name ^ ".c") in
    write_file main_path (Option.get main_content)
  end

let () =
  Format.set_margin 100;
  Asttypes.set_file file;

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

    (* let nt = Norm.norm_file ft in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Norm ast                           */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std nt
    end;
    if !norm_only then exit 0;

    let sched_nt = Scheduling.schedule_file nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Scheduled norm ast                 */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std sched_nt
    end;
    if !sched_only then exit 0;

    let simpl_nt = List.map Simpl_dataflow.simpl_dataflow sched_nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Simplified norm ast                */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std simpl_nt
    end;
    if !simpl_only then exit 0;

    let fby_sched_nt = List.map Schedule_fby.schedule simpl_nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* FBY-Scheduled norm ast             */@.";
      Format.printf "/**************************************/@.";
      Norm_ast_printer.print_node_list_std fby_sched_nt
    end;
    if !sched_fby_only then exit 0;

    let objc_nt = List.map Objc.translate_node fby_sched_nt in
    if !verbose then begin
      Format.printf "/**************************************/@.";
      Format.printf "/* Obj-C                              */@.";
      Format.printf "/**************************************/@.";
      Objc_ast_printer.print_def_list_std objc_nt
    end;
    if !objc_only then exit 0;

    if Option.is_some !main_node && !all_nodes then begin
      eprintf "Cannot specify both a main node and -all-nodes\n";
      exit 1
    end;
    if Option.is_none !main_node && !gen_c_main && not !all_nodes then begin
      eprintf
        "Cannot generate a main.c file without a main node or -all-nodes (in case of hept-compare)\n";
      exit 1
    end;
    if Option.is_none !main_node && not !compare_hept then begin
      eprintf "Must specify a main node for simulation\n";
      exit 1
    end;

    let basename = Filename.remove_extension (Filename.basename file) in

    let h_file, c_file, main_file =
      C_printer.print_c_files objc_nt basename !gen_c_main !compare_hept !main_node
    in

    write_outputs basename !c_folder h_file c_file main_file;

    if !compile && !compare_hept then begin
      Hept_utils.compile_hept file !c_folder;
      Hept_utils.compile_c basename !c_folder
    end
    else if !compile then Hept_utils.compile_c_no_hept basename !c_folder;

    if !run_main && not !compile then begin
      eprintf "Cannot specify -run-main without -compile\n";
      exit 1
    end;

    if !run_main then begin
      printf "\n%!";
      Hept_utils.run_command (Filename.concat !c_folder "main") [];
      printf "\nALL TESTS OK\n" (* if not, an exception is thrown *)
    end; *)

    (* TODO :o *)
    exit 0
  with
  | Lexical_error s ->
      Asttypes.report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
  | Parsing.Parse_error ->
      Asttypes.report_loc (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error\n@.";
      exit 1
  | Typing.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Typing.report e;
      exit 1
  (* | Norm.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Norm.report e;
      exit 1
  | Scheduling.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Scheduling.report e;
      exit 1
  | Simpl_dataflow.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Simpl_dataflow.report e;
      exit 1
  | Schedule_fby.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Schedule_fby.report e;
      exit 1
  | Objc.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." Objc.report e;
      exit 1
  | C_utils.Error (l, e) ->
      Asttypes.report_loc l;
      eprintf "%a\n@." C_utils.report e;
      exit 1 *)
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
