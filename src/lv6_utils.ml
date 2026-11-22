type lv6_node_info = {
  header_name : string;
  source_name : string;
  ctx_type : string;
  ctx_create_fun : string; (* ctx_type* ctx_create_fun(); *)
  ctx_reset_fun : string; (* void ctx_reset_fun(ctx_type* _ctx); *)
  step_fun : string;
      (* void step_fun(input1, ..., *output1, ..., *ctx_type) (no ctx if node is memoryless) *)
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

let compile_lv6 file node output_dir =
  run_command "mkdir" [ "-p"; Filename.concat output_dir "lv6" ];
  run_command "lv6" [ file; "-n"; node; "-2c"; "-dir"; Filename.concat output_dir "lv6" ]

let compile_c basename output_dir lv6_info =
  let lv6_dir = Filename.concat output_dir "lv6" in
  run_command "gcc"
    [
      Filename.concat output_dir basename ^ ".c";
      Filename.concat output_dir "main.c";
      Filename.concat lv6_dir "lustre_consts.c";
      Filename.concat lv6_dir lv6_info.source_name;
      "-o";
      Filename.concat output_dir "main";
    ]
(* TODO: add gcc options *)

let compile_c_no_lv6 basename output_dir =
  run_command "gcc"
    [
      Filename.concat output_dir basename ^ ".c";
      Filename.concat output_dir "main.c";
      "-o";
      Filename.concat output_dir "main";
    ]

let get_lv6_node_info basename node =
  let name = basename ^ "_" ^ node in
  {
    header_name = name ^ ".h";
    source_name = name ^ ".c";
    ctx_type = name ^ "_ctx_type";
    ctx_create_fun = name ^ "_ctx_new_ctx";
    ctx_reset_fun = name ^ "_ctx_reset";
    step_fun = name ^ "_step";
  }
