include Lexer

let print_all ic =
  let rec loop () =
    let ch = input_char ic in
    Printf.printf "%c\n" ch;
    loop () in
  try loop () with End_of_file -> ()

let run ic =
  let s = scan ic in match peek s with
  | Error e -> failwith e
  | Ok tok -> Printf.printf "%d\n" tok.line

let () =
  let usage = "Usage: ung <file>" in
  let path = try Sys.argv.(1) with Invalid_argument _ -> failwith usage in
  open_in path |> run
