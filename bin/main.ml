open Printf
open Idk
open Idk.Ast
open Idk.Env
open Idk.Lexer
open Idk.Env
open Idk.Interpreter
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

let eval source print_ast =
  let chars = Lexer.tokenise source in
  (* let s = String.concat " " (List.map string_of_tokentype chars) in *)
  (* print_string s; *)
  (* print_string "\n\n"; *)
  try
    let parse_result = parse chars in
    if print_ast then (
      print_string (string_of_expr parse_result);
      print_string "\n\nExecuting...\n\n")
    else ();
    let _ = Interpreter.interpret parse_result in
    ()
  with UnexpectedSequence tokens ->
    print_string "Unexpected sequence: \n";
    print_string (String.concat " " (List.map string_of_tokentype tokens))

let eval_source path =
  let source, _ = read_to_string path in
  eval source

let eval_stdin _ =
  let rec eval_stdin_aux acc =
    try eval_stdin_aux (acc ^ "\n" ^ read_line ()) with End_of_file -> acc
  in
  eval (eval_stdin_aux "")

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
