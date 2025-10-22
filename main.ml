
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

let read_command () =
  let p1 = ">> " in
  print_string p1;
  let rec aux acc =
    flush stdout;
    let line = read_line () in
    if String.contains line ';' then
      match String.index_opt line ';' with
      | Some i when i + 1 < String.length line && line.[i + 1] = ';' ->
          let before = String.sub line 0 i in
          let acc' = if before = "" then acc else before :: acc in
          String.concat " " (List.rev acc')
      | _ ->
          aux (line :: acc)
    else begin
      aux (line :: acc)
    end
  in
  aux []
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    try
      let tm = s token (from_string (read_command ())) in
      let tyTm = typeof ctx tm in
      print_endline (string_of_term (eval tm) ^ " : " ^ string_of_ty tyTm);
      loop ctx
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

top_level_loop ()
;;

