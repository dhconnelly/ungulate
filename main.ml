include Lexer
include Parser
include Token
include Ast
open Ast

let unwrap = function
  | Error e -> failwith e
  | Ok x -> x

let test = [
  Def {
    name="fact";
    value=Lambda {
      params=["n"];
      body=If {
        cond=Call {
          fn=Ident "<";
          args=[Ident "n"; Num 2];
        };
        cons=Ident "n";
        alt=Call {
          fn=Ident "*";
          args=[
            Ident "n";
            Call {
              fn=Ident "fact";
              args=[Call {
                fn=Ident "-";
                args=[Ident "n"; Num 1]
              }]
            }
          ];
        }
      }
    }
  };
  Def {
    name="main";
    value=Lambda {
      params=[];
      body=Call {
        fn=Ident "fact";
        args=[Num 5];
      }
    }
  }
]

let run ic =
  let ast = scan ic |> unwrap |> parse |> unwrap in
  List.map print ast |> String.concat "\n\n" |> Printf.printf "%s\n"

let () =
  let usage = "Usage: ung <file>" in
  let path = try Sys.argv.(1) with Invalid_argument _ -> failwith usage in
  open_in path |> run;
  List.map print test |> String.concat "\n" |> print_endline 
