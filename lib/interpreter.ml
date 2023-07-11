open Ast
open Lexer
open Parser
open Int64
open Env
open Mtime_clock

exception FnReturn of value
exception Break of value

exception
  ControlFlowExit of value (*used internally to exit from some control flow*)

exception UnexpectedBlockReturn
exception TypeError
exception TypeErrorWithInfo of string
exception UnexpectedToken of Token.tokentype
exception UnexpectedOperator

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

and random _ _ = Literal (Num (Random.float 1.))

and parse_num _ = function
  | Literal (Str s) :: _ -> (
      try Literal (Num (float_of_string s)) with Failure _ -> Literal Null)
  | _ -> Literal Null

and typeof _ = function
  | Fun _ :: _ -> Literal (Str "function")
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
            let ast = Parser.parse tokens in
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

and cos _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.cos n))
  | _ -> raise TypeError

and sin _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.sin n))
  | _ -> raise TypeError

and tan _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.tan n))
  | _ -> raise TypeError

and asin _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.asin n))
  | _ -> raise TypeError

and acos _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.acos n))
  | _ -> raise TypeError

and atan _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.atan n))
  | _ -> raise TypeError

and atan2 _ = function
  | Literal (Num y) :: Literal (Num x) :: others ->
      Literal (Num (Float.atan2 y x))
  | _ -> raise TypeError

and pow _ = function
  | Literal (Num n) :: Literal (Num p) :: others ->
      Literal (Num (Float.pow n p))
  | _ -> raise TypeError

and sqrt _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.sqrt n))
  | _ -> raise TypeError

and ln _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.log n))
  | _ -> raise TypeError

and log2 _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.log2 n))
  | _ -> raise TypeError

and log10 _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.log10 n))
  | _ -> raise TypeError

and exp _ = function
  | Literal (Num n) :: _ -> Literal (Num (Float.exp n))
  | _ -> raise TypeError

and getenv _ = function
  | Literal (Str s) :: _ ->
      Literal
        (match Option.map (fun v -> Str v) (Sys.getenv_opt s) with
        | Some v -> v
        | _ -> Null)
  | _ -> raise TypeError

(*external function definitions, loaded at startup*)
and def_ext_funs env =
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "print" [ "value" ] print;
  def_fn "println" [ "value" ] println;
  def_fn "read_to_string" [ "path" ] read_file;
  def_fn "write_to_file" [ "path"; "contents" ] write_file;
  def_fn "random" [] random;
  def_fn "parse_num" [ "str" ] parse_num;
  def_fn "typeof" [ "value" ] typeof;
  def_fn "eval" [ "code_str" ] eval;
  def_fn "timestamp" [] timestamp;
  def_fn "flush_stdout" [] flush_stdout;
  def_fn "exec" [ "command" ] exec;
  def_fn "JSON_dot_stringify" [ "object" ] object_to_string;
  def_fn "stringify" [ "object" ] object_to_string;
  def_fn "cos" [ "num" ] cos;
  def_fn "sin" [ "num" ] sin;
  def_fn "tan" [ "num" ] tan;
  def_fn "asin" [ "num" ] asin;
  def_fn "acos" [ "num" ] acos;
  def_fn "atan" [ "num" ] atan;
  def_fn "atan2" [ "x"; "y" ] atan2;
  def_fn "pow" [ "num"; "pow" ] pow;
  def_fn "sqrt" [ "num" ] sqrt;
  def_fn "ln" [ "num" ] ln;
  def_fn "log2" [ "num" ] log2;
  def_fn "log10" [ "num" ] log10;
  def_fn "exp" [ "num" ] exp;
  def_fn "sys_getenv" [ "name" ] getenv

and def_consts env =
  let def_num name value = Env.define env name (Literal (Num value)) in
  def_num "PI" Float.pi;
  def_num "E" (Float.exp 1.);
  def_num "MAX_NUM" Float.max_float;
  def_num "MIN_NUM" Float.min_float;

  Env.define env "sys_argv"
    (Array (ref (Array.map (fun v -> Literal (Str v)) Sys.argv)))

and eval_expr env expr =
  match expr with
  | Value value -> eval_value env value
  | StmtExpr stmt -> exec_stmt env stmt
  | Block exprs -> eval_block env exprs
  | Grouping expr -> eval_expr env expr
  | If (cond, if_expr, else_expr) -> eval_if env cond if_expr else_expr
  | Unary (op, expr) -> eval_unary env op expr
  | Binary (expr_l, op, expr_r) -> eval_binary env expr_l op expr_r
  | BlockReturn _ -> raise UnexpectedBlockReturn
  | For (init, cond, update, body) -> eval_for env init cond update body
  | While (cond, body) -> eval_while env cond body
  | Call (call_expr, args) -> call env call_expr args
  | Locator (lexpr, rexpr) -> locator env lexpr rexpr
  | ObjectExpr fields -> object_expr env fields
  | ArrayExpr exprs -> eval_array env exprs
  | This -> Env.get (Option.get env.this_ref) "this"
  | ClassDecl (name, methods) ->
      Env.define_virtual env name
        (Class
           ( name,
             let fields = Hashtbl.create (List.length methods) in
             List.iter
               (fun v ->
                 match v with
                 | Value (Fun (Some name, args, body)) ->
                     Hashtbl.replace fields name
                       (ClosureFun (env, Some name, args, body))
                 | _ -> raise TypeError)
               methods;
             fields ));
      Literal Null
  | ClassInst expr -> class_inst env expr

and class_inst env expr =
  match expr with
  | Call (Value (Variable class_name), args) -> (
      match Env.get_virtual env class_name with
      | Class (_name, fns) ->
          (* Printf.printf "found class %s\n" _name; *)
          (* Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k (string_of_value v)) fns; *)
          let this_val_env = Env.create (Some env) in
          this_val_env.scope <-
            Hashtbl.of_seq
              (Seq.filter
                 (fun (name, _) -> not (String.equal name class_name))
                 (Hashtbl.to_seq fns));
          Env.define this_val_env "this"
            (Object (Some class_name, this_val_env));
          this_val_env.this_ref <- Some this_val_env;

          let instance = Object (Some class_name, this_val_env) in
          let this_ref_env =
            match Hashtbl.find_opt fns class_name with
            | Some (ClosureFun (closure_env, class_name, params, body)) ->
                let this_ref_env =
                  create_this_ref_wrapper closure_env (Some this_val_env)
                in
                call_aux this_ref_env class_name
                  (List.map (fun a -> Value (eval_expr env a)) args)
                  params body
                |> ignore;
                this_ref_env
            | Some _ -> assert false
            | None ->
                raise
                  (TypeErrorWithInfo
                     (Printf.sprintf
                        "The class %s doesnt have a constructor function"
                        class_name))
          in

          Hashtbl.iter
            (fun fname f ->
              match f with
              | ClosureFun (env, name, params, body) ->
                  Hashtbl.replace this_val_env.scope fname
                    (ClosureFun (this_ref_env, name, params, body))
              | _ -> ())
            this_val_env.scope;

          instance)
  | _ ->
      raise
        (TypeErrorWithInfo
           "Expected a single depth function-call-like syntax after 'new'")

and eval_array env exprs =
  let rec array_aux acc remaining_exprs =
    match remaining_exprs with
    | [] -> Array (ref (Array.of_list acc))
    | expr :: others ->
        array_aux (List.append acc [ eval_expr env expr ]) others
  in
  array_aux [] exprs

and object_expr env fields =
  let this_val_env = Env.create (Some env) in
  Env.define this_val_env "this" (Object (None, this_val_env));
  this_val_env.this_ref <- Some this_val_env;

  Hashtbl.iter
    (fun k v -> Env.define this_val_env k (eval_expr this_val_env v))
    fields;

  Object (None, this_val_env)

and eval_fn_args env = function
  | Call (callee, args) ->
      Call
        ( eval_fn_args env callee,
          List.map (fun a -> Value (eval_expr env a)) args )
  | others -> others

and locator env lexpr rexpr =
  let lvalue = eval_expr env lexpr in
  match lvalue with
  | Array values -> (
      let indexv rexpr =
        match rexpr with
        | Literal (Num n) -> Array.get !values (int_of_float n)
        | _ ->
            raise (TypeErrorWithInfo "Arrays can only be indexed with numbers")
      in
      match rexpr with
      | Grouping expr -> indexv (eval_expr env expr)
      | Block exprs -> indexv (eval_block env exprs)
      | Value v -> indexv v
      | _ -> raise (TypeErrorWithInfo "Arrays can only be indexed with numbers")
      )
  | Object (_, fields) -> (
      match rexpr with
      | Grouping expr ->
          Env.get_field fields
            (match try_to_str env (eval_expr env expr) with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
      | Block expr_list ->
          Env.get_field fields
            (match try_to_str env (eval_block env expr_list) with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
      | Value (Literal _) as expr ->
          Env.get_field fields
            (match try_to_str env (eval_expr env expr) with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
      (*this branch should only need to evaluate function calls or variables*)
      | expr ->
          assert (
            match expr with Call _ | Value (Variable _) -> true | _ -> false);
          let expr = eval_fn_args env expr in
          eval_expr (Env.create_object_wrapper fields) expr)
  | _ -> raise (TypeErrorWithInfo "dot operator can only be used with Objects")

and call_aux parent_env name args params body_expr =
  try
    in_new_scope parent_env (fun env ->
        List.iteri
          (fun i param ->
            match List.nth_opt args i with
            | Some value -> Env.define env param (eval_expr env value)
            | None -> Env.define env param (Literal Null))
          params;
        eval_expr env body_expr)
  with FnReturn value -> value

and call parent_env call_expr args =
  let call_val = eval_expr parent_env call_expr in

  match call_val with
  | Fun (name, params, body_expr) ->
      call_aux parent_env name args params body_expr
  | ClosureFun (parent_env, name, params, body_expr) ->
      call_aux parent_env name args params body_expr
  | ExtFun (_, _, f) -> f parent_env (List.map (eval_expr parent_env) args)
  | _ -> raise TypeError

and eval_while parent_env cond body =
  try
    in_new_scope parent_env (fun env ->
        while to_bool env (eval_expr env cond) do
          let _ =
            try eval_expr env body
            with Break value -> raise (ControlFlowExit value)
          in
          ()
        done;
        Literal Null)
  with ControlFlowExit value -> value

and eval_for parent_env init cond update body =
  try
    in_new_scope parent_env (fun env ->
        let _ = eval_expr env init in
        while to_bool env (eval_expr env cond) do
          let _ =
            try eval_expr env body
            with Break value -> raise (ControlFlowExit value)
          in
          let _ = eval_expr env update in
          ()
        done;
        Literal Null)
  with ControlFlowExit value -> value

and eval_binary env expr_l op expr_r =
  let l = eval_value env (eval_expr env expr_l) in
  let r = eval_value env (eval_expr env expr_r) in
  let lr = (l, r) in

  match op with
  | Token.Plus -> add env lr
  | Token.Minus -> sub env lr
  | Token.Star -> mult env lr
  | Token.Slash -> div env lr
  | Token.EqualEqual -> eq env lr
  | Token.BangEqual -> neq env lr
  | Token.Less -> lt env lr
  | Token.LessEqual -> lteq env lr
  | Token.Greater -> gt env lr
  | Token.GreaterEqual -> gteq env lr
  | Token.Percentage -> modulo env lr
  | Token.And -> and_op env lr
  | Token.Or -> or_op env lr
  | _ -> raise UnexpectedOperator

and and_op env (l, r) = Literal (Bool (to_bool env l && to_bool env r))
and or_op env (l, r) = if to_bool env l then l else r

and add env lr =
  let lr =
    match lr with
    | (Literal (Str _), _) as lr -> (fst lr, try_to_str env (snd lr))
    | (_, Literal (Str _)) as lr -> (try_to_str env (fst lr), snd lr)
    | others -> others
  in
  match lr with
  | Literal (Num n1), Literal (Num n2) -> Literal (Num (n1 +. n2))
  | Literal (Str s1), Literal (Str s2) ->
      Literal (Str (String.concat s1 [ ""; s2 ]))
  | Array a1, Array a2 -> Array (ref (Array.append !a1 !a2))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool (b1 || b2))
  | _ -> raise TypeError

and sub env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Num (n1 -. n2))
  | Literal (Bool _), Literal (Bool true) -> Literal (Bool false)
  | (Literal (Bool true) as l), Literal (Bool false) -> l
  | _ -> raise TypeError

and mult env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Num (n1 *. n2))
  | Array a1, Array a2 ->
      let l1, l2 = (Array.length !a1, Array.length !a2) in
      Array
        (ref
           (Array.init (l1 * l2) (fun i ->
                Array
                  (ref
                     (Array.of_list
                        [ Array.get !a1 (i mod l1); Array.get !a2 (i / l1) ])))))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool (b1 && b2))
  | _ -> raise TypeError

and div env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Num (n1 /. n2))
  | _ -> raise TypeError

and lt env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Bool (n1 < n2))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool ((not b1) && b2))
  | _ -> raise TypeError

and lteq env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Bool (n1 <= n2))
  | Literal (Bool _), Literal (Bool b2) -> Literal (Bool b2)
  | _ -> raise TypeError

and gt env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Bool (n1 > n2))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool (b1 && not b2))
  | _ -> raise TypeError

and gteq env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Bool (n1 >= n2))
  | (Literal (Bool _) as b1), Literal (Bool _) -> b1
  | _ -> raise TypeError

and eq env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Bool (Float.equal n1 n2))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool (b1 == b2))
  | Literal (Str s1), Literal (Str s2) -> Literal (Bool (String.equal s1 s2))
  | Literal Null, Literal Null -> Literal (Bool true)
  | _ -> Literal (Bool false)

and neq env = function
  | Literal (Num n1), Literal (Num n2) ->
      Literal (Bool (not (Float.equal n1 n2)))
  | Literal (Bool b1), Literal (Bool b2) -> Literal (Bool (b1 != b2))
  | Literal (Str s1), Literal (Str s2) ->
      Literal (Bool (not (String.equal s1 s2)))
  | Literal Null, Literal Null -> Literal (Bool false)
  | Literal Null, _ | _, Literal Null | _ -> Literal (Bool true)

and modulo env = function
  | Literal (Num n1), Literal (Num n2) -> Literal (Num (mod_float n1 n2))
  | _ -> raise TypeError

and eval_unary env operator expr =
  match operator with
  | Token.Bang -> Literal (Bool (not (to_bool env (eval_expr env expr))))
  | Token.Minus -> (
      match try_to_num env (eval_expr env expr) with
      | Literal (Num n) -> Literal (Num (-.n))
      | others -> raise TypeError)
  | others -> raise TypeError

and eval_if parent_env cond if_expr else_expr =
  if to_bool parent_env (eval_expr parent_env cond) then
    eval_expr parent_env if_expr
  else
    match else_expr with
    | Some else_expr -> eval_expr parent_env else_expr
    | None -> Literal Null

and eval_block parent_env exprs =
  let rec block_aux env remaining_exprs =
    match remaining_exprs with
    | [] -> Literal Null
    | [ BlockReturn expr ] -> eval_expr env expr
    | Value (Fun (Some name, params, body_expr) as f) :: others ->
        Env.define env name f;
        block_aux env others
    | expr :: others ->
        let _ = eval_expr env expr in
        block_aux env others
  in
  (* if (Hashtbl.length (env.scopes.(env.curr_scope))) > 0 then  ( *)
  (* Printf.printf "before entering scope %d => " (env.curr_scope + 1); *)
  (* Hashtbl.iter (fun a b -> Printf.printf "%s : %s \n" a (string_of_value b)) (env.scopes.(env.curr_scope));) else (); *)
  (* print_string "pushing_scope\n"; *)
  (* Printf.printf "scope_idx: %d\n" env.curr_scope; *)
  let value =
    in_new_scope parent_env (fun env ->
        if Option.is_some (Hashtbl.find_opt parent_env.scope "typeof") then
          Env.define parent_env "__child_scope__" (Object (None, env))
        else ();
        block_aux env exprs)
  in
  (* print_string "popping_scope\n"; *)
  (* Printf.printf "after exiting scope %d => "(env.curr_scope + 1); *)
  (* Hashtbl.iter (fun a b -> Printf.printf "%s : %s \n" a (string_of_value b)) (env.scopes.(env.curr_scope)); *)
  (* Printf.printf "scope_idx: %d\n" env.curr_scope; *)
  value

and exec_stmt env = function
  | Print expr ->
      Printf.printf "%s\n" (val_to_string (eval_expr env expr));
      Literal Null
  | Return expr -> raise (FnReturn (eval_expr env expr))
  | Break expr -> raise (Break (eval_expr env expr))
  | Assign (name, expr) ->
      Env.update env name (eval_expr env expr);
      Env.get env name
  | Decl (name, expr) ->
      Env.define env name (eval_expr env expr);
      Literal Null
  | LocatorAssign (Locator (lexpr, rexpr), rrexpr) ->
      locator_assign env lexpr rexpr rrexpr
  | _ -> raise TypeError

and locator_assign env lexpr rexpr rrexpr =
  let lvalue = eval_expr env lexpr in
  match lvalue with
  | Array array_ref -> (
      let indexv rexpr =
        match rexpr with
        | Literal (Num n) ->
            Array.set !array_ref (int_of_float n) (eval_expr env rrexpr);
            Array.get !array_ref (int_of_float n)
        | _ ->
            raise (TypeErrorWithInfo "Arrays can only be indexed with numbers")
      in
      match rexpr with
      | Grouping expr -> indexv (eval_expr env expr)
      | Block exprs -> indexv (eval_block env exprs)
      | Value v -> indexv v
      | _ -> raise (TypeErrorWithInfo "Arrays can only be indexed with numbers")
      )
  | Object (_, fields) as lvalue -> (
      match rexpr with
      | Grouping expr ->
          Env.set_field fields
            (match try_to_str env (eval_expr env expr) with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
            (eval_expr env rrexpr);
          lvalue
      | Block expr_list ->
          Env.set_field fields
            (match try_to_str env (eval_block env expr_list) with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
            (eval_expr env rrexpr);
          lvalue
      | Value (Literal _ as rvalue) ->
          Env.set_field fields
            (match try_to_str env rvalue with
            | Literal (Str name) -> name
            | _ ->
                raise
                  (TypeErrorWithInfo
                     "dynamic '.' access can only return string type or types \
                      that can be coerced to string"))
            (eval_expr env rrexpr);
          lvalue
      (*the conditions thats left to be handled should be variables and function calls, out of which function calls raise TypeError*)
      | Value (Variable name) ->
          Env.set_field fields name (eval_expr env rrexpr);
          lvalue
      | Call _ ->
          raise
            (TypeErrorWithInfo
               "function calls do not return variable references (yet!), so \
                assigning to a function call doesn't make sense")
      | _ -> assert false)
  | _ -> raise (TypeErrorWithInfo "dot operator can only be used with Objects")

and eval_value env value =
  match value with
  | Variable name -> eval_value env (Env.get env name)
  | Fun (name, args, body) -> ClosureFun (env, name, args, body)
  | other -> other

and try_to_num env = function
  | Variable name -> try_to_num env (Env.get env name)
  | Literal (Bool b) -> Literal (Num (if b then 1. else 0.))
  | Literal (Num _) as n -> n
  | others -> others

and try_to_str env = function
  | Literal (Str _) as s -> s
  | Object _ -> Literal (Str (Printf.sprintf "<Object object>"))
  | Array values ->
      Literal
        (Str
           (Printf.sprintf "[ %s ]"
              (String.concat ", "
                 (List.map string_of_value
                    (List.init (Array.length !values) (fun i ->
                         Array.get !values i))))))
  | ClosureFun (_, name, _, _) ->
      Literal
        (Str
           (Printf.sprintf "<fun %s [closure]>"
              (match name with Some name -> name | None -> "[anonymous]")))
  | Fun (name, _, _) ->
      Literal
        (Str
           (Printf.sprintf "<fun %s>"
              (match name with Some name -> name | None -> "[anonymous]")))
  | ExtFun (name, _, _) ->
      Literal (Str (Printf.sprintf "<fun %s [external]>" name))
  | Variable name -> try_to_str env (Env.get env name)
  | Literal (Bool b) -> Literal (Str (string_of_bool b))
  | Literal (Num n) ->
      Literal
        (Str
           (let s = string_of_float n in
            if String.ends_with s ~suffix:"." then
              String.sub s 0 (String.length s - 1)
            else s))
  | Literal Null -> Literal (Str "null")

and to_bool env = function
  | Fun _ -> true
  | ClosureFun _ -> true
  | Object (_, fields) ->
      Hashtbl.length fields.scope > 1 (*ignore the 'this' field*)
  | ExtFun _ -> true
  | Variable name -> to_bool env (Env.get env name)
  | Array values -> Array.length !values > 0
  | Literal (Bool b) -> b
  | Literal (Str s) -> String.length s >= 1
  | Literal (Num n) -> n >= 1.
  | Literal Null -> false

and in_new_scope env (f : Env.env -> 'a) =
  let env = Env.create (Some env) in
  let value = f env in
  value
