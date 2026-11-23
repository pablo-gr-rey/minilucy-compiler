type hept_node_info = {
  header_name : string;
  source_name : string;
  mem_type : string;
  out_type : string;
  reset_fun : string; (* void File__node_reset(mem* self); *)
  step_fun : string;
      (* void File__node_step(input1, ..., out_type* output, mem_type* self) (no mem if node is memoryless) *)
}

let run_command prog args =
  let argv = Array.of_list (prog :: args) in
  let pid = Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr in
  Printf.printf "Running: %s\n%!" (String.concat " " (prog :: args));
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> failwith (Printf.sprintf "%s exited with code %d" prog n)
  | Unix.WSIGNALED n -> failwith (Printf.sprintf "%s killed by signal %d" prog n)
  | Unix.WSTOPPED n -> failwith (Printf.sprintf "%s stopped by signal %d" prog n)

let compile_hept file output_dir =
  let basename = Filename.remove_extension (Filename.basename file) in
  run_command "mkdir" [ "-p"; Filename.concat output_dir "hept" ];
  run_command "heptc" [ "-target"; "c"; "-targetpath"; Filename.concat output_dir "hept"; file ];
  run_command "rm" [ basename ^ ".epci"; basename ^ ".log"; basename ^ ".mls" ]

let compile_c basename output_dir =
  let hept_dir = Filename.concat (Filename.concat output_dir "hept") basename ^ "_c" in
  let hept_lib =
    let ic = Unix.open_process_in "heptc -where" in
    let path = input_line ic in
    ignore (Unix.close_process_in ic);
    Filename.concat path "c"
  in
  run_command "gcc"
    [
      Filename.concat output_dir basename ^ ".c";
      Filename.concat output_dir "main.c";
      Filename.concat hept_dir basename ^ "_types.c";
      Filename.concat hept_dir basename ^ ".c";
      "-o";
      Filename.concat output_dir "main";
      "-iquote" ^ hept_lib;
    ]
(* TODO: add gcc options *)

let compile_c_no_hept basename output_dir =
  run_command "gcc"
    [
      Filename.concat output_dir basename ^ ".c";
      Filename.concat output_dir "main.c";
      "-o";
      Filename.concat output_dir "main";
    ]

let get_hept_node_info basename node =
  let name = String.capitalize_ascii basename ^ "__" ^ node in
  {
    header_name = Filename.concat (basename ^ "_c") basename ^ ".h";
    source_name = Filename.concat (basename ^ "_c") basename ^ ".c";
    mem_type = name ^ "_mem";
    out_type = name ^ "_out";
    reset_fun = name ^ "_reset";
    step_fun = name ^ "_step";
  }
