open Ast
open Lexer
open Parser
open Int64
open Env
open Mtime_clock
open LangCore
open Exceptions
open Eval

let read_to_string path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let rec interpret expr =
  let env = Env.create None in
  def_ext_funs env;
  def_consts env;
  Env.define env "Array" (gen_array_obj (Some env));
  Env.define env "Math" (gen_math_obj (Some env));
  Env.define env "globalThis" (Object (Some "GlobalEnvironment", env));
  try eval_expr env expr
  with FnReturn value ->
    Printf.printf
      "\n\
       =================================\n\
       Returned out of root block with value: %s\n"
      (string_of_value value);
    Literal Null

and print _ = function
  | x :: _ ->
      print_string (val_to_string x);
      Literal Null
  | _ -> Literal Null

and println _ = function
  | x :: _ ->
      Printf.printf "%s\n" (val_to_string x);
      Literal Null
  | [] ->
      print_string "\n";
      Literal Null

and read_file _ = function
  | Literal (Str path) :: _ -> Literal (Str (read_to_string path))
  | _ -> Literal Null

and write_file env args =
  match args with
  | Literal (Str path) :: content :: _ ->
      let out_channel = open_out path in
      output_string out_channel (val_to_string content);
      close_out out_channel;
      Literal Null
  | _ -> Literal Null

and parse_num _ = function
  | Literal (Str s) :: _ -> (
      try Literal (Num (float_of_string s)) with Failure _ -> Literal Null)
  | _ -> Literal Null

and typeof _ = function
  | Fun _ :: _ -> Literal (Str "function")
  | Array _ :: _ -> Literal (Str "array")
  | Object _ :: _ -> Literal (Str "object")
  | ClosureFun _ :: _ -> Literal (Str "closure")
  | ExtFun _ :: _ -> Literal (Str "external_function")
  | Variable _ :: _ -> Literal (Str "unevaluated_variable")
  | Literal (Num _) :: _ -> Literal (Str "number")
  | Literal (Str _) :: _ -> Literal (Str "string")
  | Literal (Bool _) :: _ -> Literal (Str "boolean")
  | Literal Null :: _ -> Literal (Str "null")
  | _ -> Literal Null

and eval env = function
  | l :: _ -> (
      match try_to_str env l with
      | Literal (Str s) -> (
          try
            let tokens = Lexer.tokenise s in
            let ast = Result.get_ok (Parser.parse tokens) in
            eval_expr env ast
          with _ -> Literal Null)
      | _ -> Literal Null)
  | _ -> Literal Null

and timestamp _ _ =
  let t = Mtime_clock.elapsed () in
  let t = Mtime.Span.to_float_ns t in
  Literal (Num (t *. 1e-9))

and flush_stdout _ _ =
  flush stdout;
  Literal Null

and exec _ = function
  | Literal (Str s) :: _ -> Literal (Num (float_of_int (Sys.command s)))
  | _ -> Literal Null

and object_to_string _ = function
  | (Object _ as obj) :: _ -> Literal (Str (Ast.obj_to_string [] obj))
  | _ -> raise TypeError

and getenv _ = function
  | Literal (Str s) :: _ ->
      Literal
        (match Option.map (fun v -> Str v) (Sys.getenv_opt s) with
        | Some v -> v
        | _ -> Null)
  | _ -> raise TypeError

and string_to_ascii _ = function
  | Literal (Str s) :: _ ->
      Array
        (ref
           (Array.of_seq
              (Seq.map
                 (fun c -> Literal (Num (float_of_int (Char.code c))))
                 (String.to_seq s))))
  | _ -> raise TypeError

and ascii_to_string _ = function
  | Array elts :: _ ->
      Literal
        (Str
           (String.of_seq
              (Array.to_seq
                 (Array.map
                    (fun e ->
                      match e with
                      | Literal (Num n) -> Char.chr (int_of_float n)
                      | _ -> raise TypeError)
                    !elts))))
  | _ -> raise TypeError

(*external function definitions, loaded at startup*)
and def_ext_funs env =
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "print" [ "value" ] print;
  def_fn "println" [ "value" ] println;
  def_fn "read_to_string" [ "path" ] read_file;
  def_fn "write_to_file" [ "path"; "contents" ] write_file;
  def_fn "parse_num" [ "str" ] parse_num;
  def_fn "typeof" [ "value" ] typeof;
  def_fn "eval" [ "code_str" ] eval;
  def_fn "timestamp" [] timestamp;
  def_fn "flush_stdout" [] flush_stdout;
  def_fn "exec" [ "command" ] exec;
  def_fn "JSON_dot_stringify" [ "object" ] object_to_string;
  def_fn "stringify" [ "object" ] object_to_string;
  def_fn "sys_getenv" [ "name" ] getenv;
  def_fn "stoa" [ "string" ] string_to_ascii;
  def_fn "atos" [ "ascii" ] ascii_to_string

and def_consts env =
  let def_num name value = Env.define env name (Literal (Num value)) in
  def_num "PI" Float.pi;
  def_num "E" (Float.exp 1.);
  def_num "MAX_NUM" Float.max_float;
  def_num "MIN_NUM" Float.min_float;

  Env.define env "sys_argv"
    (Array (ref (Array.map (fun v -> Literal (Str v)) Sys.argv)))
