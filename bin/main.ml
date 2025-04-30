open Cnf;;

let tokensListFromString (str : string) = Str.split (Str.regexp_string " ") str;;

(* let buildCNFFromString (str : string) = buildCNF (tokensListFromString str);; *)

let satisfyFromString (str : string) = satisfy (tokensListFromString str);;

(* (string * bool) list *)
let printSatis (input : (string * bool) list) : unit =
  let f a = match a with (st, b) ->
    if b then (
      print_string st;
      print_string " ";
    )
    else
      print_string "_ ";
  in ignore (List.map f input);
  print_endline ""
;;

let rec prompt () =
  print_endline "\nEnter a boolean expression in CNF format (use spaces between tokens):";
  print_endline "Example: a AND NOT b";
  print_endline "Type 'quit' to exit";
  print_string "> ";
  match read_line () with
  | "quit" -> print_endline "Goodbye!"; exit 0
  | input ->
      let result = satisfyFromString input in
      (match result with
      | [("error", false)] -> print_endline "Expression is not satisfiable"
      | _ ->
          print_endline "Expression is satisfiable with the following assignment:";
          printSatis result);
          prompt ()
;;

let () = prompt ()