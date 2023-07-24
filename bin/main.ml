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

let rec report_line file_name lines line_number start_idx =
  let line_num_str = string_of_int line_number in
  let line_num_len = String.length line_num_str in
  Printf.printf "-> %s\n" file_name;
  Printf.printf " %s|\n" (String.make line_num_len ' ');
  Printf.printf " %s| %s\n" line_num_str (List.nth lines line_number);
  Printf.printf " %s| %s^%s\n" (String.make line_num_len ' ') (String.make start_idx ' ') (String.make (((List.nth lines line_number) |> String.length)-start_idx) '^')

and eval source_filename source print_ast =
  let lines = String.split_on_char '\n' source in
  let tokens = Lexer.tokenise source in
  (* let s = String.concat " " (List.map string_of_tokentype chars) in *)
  (* print_string s; *)
  (* print_string "\n\n"; *)
  try
    let parse_result = parse tokens in
    if print_ast then (
      print_string (string_of_expr parse_result);
      print_string "\n\nExecuting...\n\n")
    else ();
    let _ = Interpreter.interpret parse_result in
    ()
  with UnexpectedSequence tokens ->
    print_string "Unexpected sequence: \n";
    let line_number, error_start  =
      match tokens with token :: _ -> (token.line, match token.col with | (start, _) -> start) | _ -> (List.length lines, 0)
    in
    report_line source_filename lines line_number error_start
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
