let print_all ic =
  let rec loop () =
    let ch = input_char ic in
    Printf.printf "%c\n" ch;
    loop () in
  try loop () with End_of_file -> ()

let run ic =
  print_all ic

let () =
  let usage = "Usage: ung <file>" in
  let path = try Sys.argv.(1) with Invalid_argument _ -> failwith usage in
  open_in path |> run
