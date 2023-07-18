open Env
open Token

(*for now. TODO: String interning*)
let repeat s n =
  let rec helper s1 n1 = if n1 = 0 then s1 else helper (s1 ^ s) (n1 - 1) in
  helper "" n

let rec string_of_some_expr = function
  | None -> "None"
  | Some expr -> string_of_expr expr

and string_of_expr (e : expr) =
  match e with
  | If (cond, if_expr, else_expr) ->
      Printf.sprintf "if( %s ) { %s } else{ %s }" (string_of_expr cond)
        (string_of_expr if_expr)
        (string_of_some_expr else_expr)
  | While (cond, while_expr) ->
      Printf.sprintf "while( %s ) { %s }" (string_of_expr cond)
        (string_of_expr while_expr)
  | For (init, cond, update, body) ->
      Printf.sprintf "for( %s; %s; %s ) { %s }" (string_of_expr init)
        (string_of_expr cond) (string_of_expr update) (string_of_expr body)
  | Binary (l_expr, op, r_expr) ->
      Printf.sprintf "binary( %s %s %s )" (string_of_expr l_expr)
        (string_of_tokentype op) (string_of_expr r_expr)
  | Unary (op, r_expr) ->
      Printf.sprintf "unary( %s%s )" (string_of_tokentype op)
        (string_of_expr r_expr)
  | Grouping group_expr ->
      Printf.sprintf "group( %s )" (string_of_expr group_expr)
  | Call (ident_expr, args) ->
      Printf.sprintf "call( %s( %s ) )"
        (string_of_expr ident_expr)
        (String.concat ", " (List.map string_of_expr args))
  | StmtExpr stmt -> string_of_stmt stmt
  | Block exprs ->
      Printf.sprintf "block( %s )"
        (String.concat "; " (List.map string_of_expr exprs))
  | ArrayExpr exprs ->
      Printf.sprintf "array ( %s )"
        (String.concat ", " (List.map string_of_expr exprs))
  | Value v -> string_of_value v
  | BlockReturn e -> Printf.sprintf "block_return( %s )" (string_of_expr e)
  | Locator (el, er) ->
      Printf.sprintf "locator( %s -> %s )" (string_of_expr el)
        (string_of_expr er)
  | This -> "this"
  | ObjectExpr fields ->
      "object{ "
      ^ Hashtbl.fold
          (fun k v acc ->
            acc ^ Printf.sprintf "field( %s : %s )" k (string_of_expr v) ^ ", ")
          fields ""
      ^ " }"
  | ClassDecl (name, parents, fields) ->
      Printf.sprintf "class %s [extends: %s] { %s }" name
        (String.concat "," parents)
        (String.concat ", " (List.map string_of_expr fields))
  | ClassInst expr -> Printf.sprintf "new( %s )" (string_of_expr expr)

and string_of_stmt (s : stmt) =
  match s with
  | Return expr -> Printf.sprintf "return( %s );" (string_of_expr expr)
  | Break expr -> Printf.sprintf "break ( %s );" (string_of_expr expr)
  | Print expr -> Printf.sprintf "print( %s );" (string_of_expr expr)
  | Decl (name, expr) ->
      Printf.sprintf "let %s = %s;" name (string_of_expr expr)
  | Assign (name, expr) -> Printf.sprintf "%s = %s;" name (string_of_expr expr)
  | LocatorAssign (lexpr, expr) ->
      Printf.sprintf "loc %s = %s;" (string_of_expr lexpr) (string_of_expr expr)

and string_of_value (v : value) =
  match v with
  | Fun (name, params, body_expr) ->
      Printf.sprintf "fun %s( %s ) { %s }"
        (match name with Some name -> name | None -> "[anonymous]")
        (String.concat ", " params)
        (string_of_expr body_expr)
  | ClosureFun (_, name, params, body_expr) ->
      Printf.sprintf "fun %s( %s ) [closure] { %s }"
        (match name with Some name -> name | None -> "[anonymous]")
        (String.concat ", " params)
        (string_of_expr body_expr)
  | ExtFun (name, params, f) ->
      Printf.sprintf "fun %s( %s ) { [external] }" name
        (String.concat ", " params)
  | Variable name -> Printf.sprintf "(var %s)" name
  | Array values ->
      Printf.sprintf "[ %s ]"
        (String.concat ", " (List.map string_of_value (Array.to_list !values)))
  | Literal l -> string_of_literal l
  | Object _ as obj -> obj_to_string [] obj

and string_of_literal (l : literal) =
  match l with
  | Num n ->
      let s = string_of_float n in
      if String.ends_with s ~suffix:"." then String.sub s 0 (String.length s - 1)
      else s
  | Str s -> Printf.sprintf "\"%s\"" s
  | Bool b -> string_of_bool b
  | Null -> "null"

and val_to_string (v : value) =
  match v with
  | Object _ -> "<Object object>"
  | Fun (name, params, _) ->
      Printf.sprintf "<fun %s(%s)>"
        (match name with Some name -> name | None -> "[anonymous]")
        (String.concat ", " params)
  | ClosureFun (_, name, params, _) ->
      Printf.sprintf "<fun %s(%s) [closure]>"
        (match name with Some name -> name | None -> "[anonymous]")
        (String.concat ", " params)
  | ExtFun (name, params, _) ->
      Printf.sprintf "<fun %s(%s) [external]>" name (String.concat ", " params)
  | Variable name -> Printf.sprintf "<var %s>" name
  | Array values ->
      Printf.sprintf "[ %s ]"
        (String.concat ", " (List.map val_to_string (Array.to_list !values)))
  | Literal l -> (
      match l with
      | Num n ->
          let s = string_of_float n in
          if String.ends_with s ~suffix:"." then
            String.sub s 0 (String.length s - 1)
          else s
      | Str s -> s
      | Bool b -> string_of_bool b
      | Null -> "null")

and obj_to_string visited_envs obj =
  let rec obj_to_string_aux indent visited_envs obj =
    match obj with
    | Object (name, field_env) ->
        if Option.is_some (List.find_opt (fun e -> e == field_env) visited_envs)
        then "<Recursive Object>"
        else
          let visited_envs = List.concat [ [ field_env ]; visited_envs ] in
          Printf.sprintf "%s{%s\n%s}"
            (match name with Some name -> name | _ -> "[Object]")
            (Hashtbl.fold
               (fun k v acc ->
                 if String.equal k "this" then acc
                 else
                   acc
                   ^ Printf.sprintf "\n%s\": %s,"
                       (String.concat "\n"
                          (List.mapi
                             (fun i v ->
                               repeat " " (indent + 2)
                               ^ (if i <= 0 then "\"" else "")
                               ^ v)
                             (String.split_on_char '\n' k)))
                       (obj_to_string_aux (indent + 2) visited_envs v))
               field_env.scope "")
            (repeat " " indent)
    | Fun (name, params, _) ->
        Printf.sprintf "<fun %s(%s)>"
          (match name with Some name -> name | None -> "[anonymous]")
          (String.concat ", " params)
    | ClosureFun (_, name, params, _) ->
        Printf.sprintf "<fun %s(%s) [closure]>"
          (match name with Some name -> name | None -> "[anonymous]")
          (String.concat ", " params)
    | ExtFun (name, params, _) ->
        Printf.sprintf "<fun %s(%s) [external]>" name
          (String.concat ", " params)
    | Variable name -> Printf.sprintf "<var %s>" name
    | Array values ->
        Printf.sprintf "[ %s ]"
          (String.concat ", "
             (List.map
                (obj_to_string_aux (indent + 2) visited_envs)
                (List.init (Array.length !values) (fun i -> Array.get !values i))))
    | Literal l -> (
        match l with
        | Num n ->
            let s = string_of_float n in
            if String.ends_with s ~suffix:"." then
              String.sub s 0 (String.length s - 1)
            else s
        | Str s -> Printf.sprintf "\"%s\"" s
        | Bool b -> string_of_bool b
        | Null -> "null")
  in

  obj_to_string_aux 0 visited_envs obj
