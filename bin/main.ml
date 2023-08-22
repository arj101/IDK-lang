open Printf
open Idk
open Idk.Ast
open Idk.Env
open Idk.Lexer
open Idk.Env
open Idk.Token
open Idk.Parser
open Mtime
open Mtime_clock
open AnsiFormatter
open Printf

let read_to_string path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = (really_input_string ic len, len) in
  close_in ic;
  content

exception UnexpectedCharacter of char
exception UnexpectedEof
exception Unimplemented of string
exception ParsedVal
exception ParseResult of expr

let rec report_error file_name lines (error : parse_error) =
  let line_number = error.token.span.line
  and start_idx = error.token.span.col in
  let line_num_str = string_of_int (line_number + 1) in
  let line_num_len = String.length line_num_str in

  print_string "-> ";
  Printf.sprintf "%s\n" file_name |> print_styled [ fg_color Cyan ];
  print_styled [ text_format Bold; fg_color Red ] "error: ";
  Printf.sprintf "%s\n" error.info |> print_styled [ text_format Bold ];

  print_endline
    (Printf.sprintf " %s|" (String.make line_num_len ' ')
    |> styled [ fg_color Cyan ]);
  Printf.printf "%s %s\n"
    (Printf.sprintf " %s|" line_num_str |> styled [ fg_color Cyan ])
    (List.nth lines line_number);
  Printf.printf "%s %s%s\n"
    (Printf.sprintf " %s|" (String.make line_num_len ' ')
    |> styled [ fg_color Cyan ])
    (String.make (start_idx) ' ')
    (String.make (error.token.span.length) '^' |> styled [ fg_color Red ])

let rec report_errors source_name lines (errors : parse_error list) =
  List.iter
    (fun e ->
      report_error source_name lines e;
      print_newline ())
    errors

and eval source_filename source print_ast =
  let regex = Str.regexp "\t" in
  let source = Str.global_replace regex "  " source in
  let lines = String.split_on_char '\n' source in
  let tokens = Lexer.tokenise source in
  (* let s = String.concat " " (List.map string_of_tokentype chars) in *)
  (* print_string s; *)
  (* print_string "\n\n"; *)
  match parse tokens with
  | Ok parse_result ->
      if print_ast then (
        print_string (string_of_expr parse_result);
        print_string "\n\nExecuting...\n\n")
      else ();
      let _ = Interpreter.interpret parse_result in
      ()
  | Error errors -> report_errors source_filename lines errors
(* print_string (String.concat " " (List.map string_of_token tokens)) *)

and eval_source path =
  let source, _ = read_to_string path in
  eval path source

and eval_stdin _ =
  let rec eval_stdin_aux acc =
    try eval_stdin_aux (acc ^ "\n" ^ read_line ()) with End_of_file -> acc
  in
  eval "[stdin]" (eval_stdin_aux "")

let () =
  let print_ast =
    if
      List.length
        (List.filter (fun s -> String.equal s "-ast") (Array.to_list Sys.argv))
      > 0
    then true
    else false
  in
  if Array.length Sys.argv >= 2 then eval_source Sys.argv.(1) print_ast
  else eval_stdin () print_ast
