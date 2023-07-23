open Token
open Ast
open Env

exception UnexpectedToken of tokentype
exception UnexpectedEof
exception UnexpectedSequence of token list
exception UnexpectedSyntaxTree
exception UnexpectedSyntaxTreeWithInfo of string

let identity tokens = (tokens, Value (Literal Null))

let rec parse (tokens : token list) : expr =
  let rec parse_aux tokens_remaining expr_accum =
    match tokens_remaining with
    | [] -> expr_accum (*ignore newlines and semicolons*)
    | { t = Newline } :: others | { t = Semicolon } :: others ->
        parse_aux others expr_accum
    | tokens ->
        let tokens_remaining, expr = expression tokens_remaining in
        parse_aux tokens_remaining (expr :: expr_accum)
  in
  let exprs = parse_aux tokens [] in
  Block (List.rev exprs)

and expression tokens =
  match tokens with
  | { t = Newline } :: others -> expression others
  | { t = Let } :: others -> let_binding others
  (* | Ident name :: others ->  *)
  (*     match consume_newlines others with *)
  (*     | simple_assignment tokens *)
  (* (* | Ident name :: _ as tokens -> assignment_or_topexpr tokens *) *)
  | { t = Print } :: others -> print others
  | { t = Return } :: others -> return others
  | { t = Break } :: others -> break others
  | { t = If } :: others -> if_expr others
  | { t = Fun } :: others -> fun_expr others
  | { t = While } :: others ->
      while_expr others
      (* | LeftBrace :: others -> maybe_object others *)
      (*BOTH objects and scopes have high precedence because they can be used with other operators*)
  | { t = For } :: others -> for_expr others
  | { t = Class } :: others -> class_decl others
  | others -> maybe_assignment others

and class_decl tokens =
  let remaining_tokens, ident =
    match tokens with
    | { t = Ident name } :: others -> (others, name)
    | others -> raise (UnexpectedSequence others)
  in

  let rec parse_parents remaining_tokens acc =
    match consume_newlines remaining_tokens with
    | { t = Ident name } :: others -> parse_parents others (name :: acc)
    | { t = Comma } :: others -> parse_parents others acc
    | { t = LeftBrace } :: _ as others -> (others, acc)
    | others -> raise (UnexpectedSequence others)
  in

  let remaining_tokens, parents =
    match consume_newlines remaining_tokens with
    | { t = Extends } :: others -> parse_parents others []
    | others -> (others, [])
  in

  match consume_newlines remaining_tokens with
  | { t = LeftBrace } :: others ->
      let rec get_functions tokens_remaining acc =
        match consume_newlines tokens_remaining with
        | { t = Ident name } :: others as tokens ->
            if String.equal name ident then
              let tokens_remaining, expr = fun_expr tokens in
              get_functions tokens_remaining (List.append acc [ expr ])
            else (
              print_string
                "Only constructors can be declared without 'fun' keyword\n";
              raise (UnexpectedSequence tokens))
        | { t = Fun } :: others ->
            let tokens_remaining, expr = fun_expr others in
            (match expr with
            | Value (Fun (Some name, _, _)) ->
                if String.equal name ident then
                  raise
                    (UnexpectedSyntaxTreeWithInfo
                       "Constructor function should start without a 'fun' \
                        keyword")
                else ()
            | _ -> assert false);
            get_functions tokens_remaining (List.append acc [ expr ])
        | { t = RightBrace } :: others -> (others, acc)
        | others -> raise (UnexpectedSequence others)
      in
      let remaining_toknes, fns = get_functions others [] in
      (remaining_toknes, ClassDecl (ident, parents, fns))
  | others -> raise (UnexpectedSequence others)

and maybe_object tokens =
  let remaining_tokens = consume_newlines tokens in
  match remaining_tokens with
  | { t = Ident name } :: others | { t = Str name } :: others -> (
      match consume_newlines others with
      | { t = Colon } :: others -> object_expr tokens
      | _ -> block_expr tokens)
  | { t = RightBrace } :: _ -> object_expr tokens
  | _ -> block_expr tokens

and object_expr tokens =
  let rec consume_comma = function
    | { t = Newline } :: others -> consume_comma others
    | { t = Comma } :: others -> others
    | others -> others
  in

  let rec aux_loop acc remaining_tokens =
    match consume_newlines remaining_tokens with
    | { t = Ident name } :: others | { t = Str name } :: others -> (
        match consume_newlines others with
        | { t = Colon } :: others ->
            let remaining_tokens, rexpr = expression others in
            let remaining_tokens = consume_comma remaining_tokens in
            Hashtbl.replace acc name rexpr;
            aux_loop acc remaining_tokens
        | others -> raise (UnexpectedSequence others))
    | { t = RightBrace } :: others -> (others, ObjectExpr acc)
    | others -> raise (UnexpectedSequence others)
  in
  aux_loop (Hashtbl.create 16) tokens

and expand_shorthand_assign operator lexpr rexpr =
  match operator with
  | { t = PlusEqual } as op ->
      Binary (lexpr, { t = Plus; line = op.line; col = op.col }, rexpr)
  | { t = MinusEqual } as op ->
      Binary (lexpr, { t = Minus; line = op.line; col = op.col }, rexpr)
  | { t = StarEqual } as op ->
      Binary (lexpr, { t = Star; line = op.line; col = op.col }, rexpr)
  | { t = SlashEqual } as op ->
      Binary (lexpr, { t = Slash; line = op.line; col = op.col }, rexpr)
  | { t = PercentageEqual } as op ->
      Binary (lexpr, { t = Percentage; line = op.line; col = op.col }, rexpr)
  | _ -> assert false

and maybe_assignment tokens =
  try
    let remaining_tokens, lexpr = locator tokens in
    match remaining_tokens with
    | { t = Equal } :: others -> (
        let remaining_tokens, rexpr = expression others in
        match lexpr with
        | Value (Variable name) ->
            (remaining_tokens, StmtExpr (Assign (name, rexpr)))
        | Locator _ ->
            (remaining_tokens, StmtExpr (LocatorAssign (lexpr, rexpr)))
        | _ -> assert false)
    | ({ t = PlusEqual } as op) :: others
    | ({ t = MinusEqual } as op) :: others
    | ({ t = StarEqual } as op) :: others
    | ({ t = SlashEqual } as op) :: others
    | ({ t = PercentageEqual } as op) :: others -> (
        let remaining_tokens, rexpr = expression others in
        match lexpr with
        | Value (Variable name) ->
            ( remaining_tokens,
              StmtExpr (Assign (name, expand_shorthand_assign op lexpr rexpr))
            )
        | Locator _ ->
            ( remaining_tokens,
              StmtExpr
                (LocatorAssign (lexpr, expand_shorthand_assign op lexpr rexpr))
            )
        | _ -> assert false)
    | others -> logic_or tokens (*expensive backtracking, idk how to fix this*)
  with UnexpectedSequence _ -> logic_or tokens

and consume_newlines = function
  | { t = Newline } :: others -> consume_newlines others
  | others -> others

and let_binding tokens =
  let remaining_tokens, ident, value =
    match tokens with
    (*let [name] = expression*)
    | { t = Ident name } :: others ->
        let rec let_eq = function
          (*ignore newlines*)
          | { t = Newline } :: others -> let_eq others
          (*let name [=]  expression*)
          | { t = Equal } :: others ->
              let rec let_expr = function
                (*ignore newlines*)
                | { t = Newline } :: others -> let_expr others
                (*let name = [expression]*)
                | tokens ->
                    let remaining_tokens, value = expression tokens in
                    (remaining_tokens, name, value)
              in
              let_expr others
          (*[let name], no expression*)
          | others -> (others, name, Value (Literal Null))
        in
        let_eq others
    | others -> raise (UnexpectedSequence others)
  in
  (remaining_tokens, StmtExpr (Decl (ident, value)))

and for_expr tokens =
  let remaining_tokens, init_expr = expression tokens in

  let remaining_tokens =
    match remaining_tokens with
    | { t = Semicolon } :: others -> others
    | others -> others
  in

  let remaining_tokens, condition_expr = expression remaining_tokens in

  let remaining_tokens =
    match remaining_tokens with
    | { t = Semicolon } :: others -> others
    | others -> others
  in

  let remaining_tokens, update_expr = expression remaining_tokens in

  let remaining_tokens =
    match remaining_tokens with
    | { t = Semicolon } :: others -> others
    | others -> others
  in

  let remaining_tokens =
    match remaining_tokens with
    | { t = Do } :: others -> others
    | others -> others
  in

  let remaining_tokens, body_expr = expression remaining_tokens in

  (remaining_tokens, For (init_expr, condition_expr, update_expr, body_expr))

and print tokens =
  let remaining_tokens, expr = expression tokens in
  (remaining_tokens, StmtExpr (Print expr))

and return tokens =
  let remaining_tokens, expr = expression tokens in
  (remaining_tokens, StmtExpr (Return expr))

and break tokens =
  let remaining_tokens, expr = expression tokens in
  (remaining_tokens, StmtExpr (Break expr))

and if_expr tokens =
  let remaining_tokens, cond_expr = expression tokens in
  let remaining_tokens =
    match remaining_tokens with
    | { t = Then } :: others -> others
    | others -> others
  in
  let remaining_tokens, if_expr = expression remaining_tokens in

  let rec else_expr remaining_tokens =
    match remaining_tokens with
    | { t = Newline } :: others -> else_expr others
    | { t = Else } :: others ->
        let remaining_tokens, else_expr = expression others in
        (remaining_tokens, If (cond_expr, if_expr, Some else_expr))
    | others -> (others, If (cond_expr, if_expr, None))
  in

  else_expr remaining_tokens

and fun_expr tokens =
  let remaining_tokens, name_ident =
    match tokens with
    | { t = Ident name } :: others -> (others, Some name)
    | others -> (others, None)
  in

  let remaining_tokens =
    match remaining_tokens with
    | { t = LeftParen } :: others -> others
    | others -> raise (UnexpectedSequence others)
  in

  let rec args remaining_tokens acc =
    match remaining_tokens with
    | { t = Newline } :: others | { t = Comma } :: others -> args others acc
    | { t = RightParen } :: others -> (others, acc)
    | { t = Ident name } :: others -> args others (name :: acc)
    | others -> raise (UnexpectedSequence others)
  in

  let remaining_tokens, ident_list = args remaining_tokens [] in
  let remaining_tokens = consume_newlines remaining_tokens in

  let remaining_tokens, body_expr =
    match remaining_tokens with
    | { t = Colon } :: others -> expression others
    | { t = LeftBrace } :: others -> block_expr others
    | others -> raise (UnexpectedSequence others)
  in

  (remaining_tokens, Value (Fun (name_ident, List.rev ident_list, body_expr)))

and while_expr tokens =
  let remaining_tokens, cond_expr = expression tokens in

  let remaining_tokens =
    match remaining_tokens with
    | { t = Do } :: others -> others
    | others -> others
  in

  let remaining_tokens, loop_expr = expression remaining_tokens in
  (remaining_tokens, While (cond_expr, loop_expr))

and block_expr tokens =
  let rec block_aux tokens_remaining acc =
    match tokens_remaining with
    | [] -> raise UnexpectedEof
    | { t = Newline } :: others | { t = Semicolon } :: others ->
        block_aux others acc
    | { t = RightBrace } :: others -> (others, acc)
    | others ->
        let tokens_remaining, expr = expression others in
        block_aux tokens_remaining (expr :: acc)
  in
  let others, exprs = block_aux tokens [] in
  let exprs =
    match exprs with
    | last_expr :: tail -> BlockReturn last_expr :: tail
    | [] -> []
  in
  (others, Block (List.rev exprs))

and ignore_newlines (tokens, expr) =
  match tokens with
  | { t = Newline } :: others -> (others, expr)
  | others -> (others, expr)

and logic_or tokens =
  let rec or_aux tokens_remaining acc prev_op =
    let tokens_remaining, expr = logic_and tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = Or } as operator) :: others ->
        or_aux others (Binary (acc, prev_op, expr)) operator
    | others -> (others, Binary (acc, prev_op, expr))
  in
  let remaining_tokens, leftmost_expr = logic_and tokens in
  match consume_newlines remaining_tokens with
  | ({ t = Or } as operator) :: others -> or_aux others leftmost_expr operator
  | others -> (others, leftmost_expr)

and logic_and tokens =
  let rec and_aux tokens_remaining acc prev_op =
    let tokens_remaining, expr = equality tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = And } as op) :: others ->
        and_aux others (Binary (acc, prev_op, expr)) op
    | others -> (others, Binary (acc, prev_op, expr))
  in
  let remaining_tokens, leftmost_expr = equality tokens in
  match consume_newlines remaining_tokens with
  | ({ t = And } as op) :: others -> and_aux others leftmost_expr op
  | others -> (others, leftmost_expr)

and equality tokens =
  let rec eq_aux tokens_remaining acc prev_operator =
    let tokens_remaining, expr = comparison tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = BangEqual } as operator) :: others
    | ({ t = EqualEqual } as operator) :: others ->
        eq_aux others (Binary (acc, prev_operator, expr)) operator
    | others -> (others, Binary (acc, prev_operator, expr))
  in
  let remaining_tokens, leftmost_expr = comparison tokens in
  match consume_newlines remaining_tokens with
  | ({ t = BangEqual } as operator) :: others
  | ({ t = EqualEqual } as operator) :: others ->
      eq_aux others leftmost_expr operator
  | others -> (others, leftmost_expr)

and comparison tokens =
  let rec cmp_aux tokens_remaining acc prev_operator =
    let tokens_remaining, expr = term tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = Less } as op) :: others
    | ({ t = LessEqual } as op) :: others
    | ({ t = Greater } as op) :: others
    | ({ t = GreaterEqual } as op) :: others ->
        cmp_aux others (Binary (acc, prev_operator, expr)) op
    | others -> (others, Binary (acc, prev_operator, expr))
  in
  let remaining_tokens, leftmost_expr = term tokens in
  match consume_newlines remaining_tokens with
  | ({ t = Less } as operator) :: others
  | ({ t = LessEqual } as operator) :: others
  | ({ t = Greater } as operator) :: others
  | ({ t = GreaterEqual } as operator) :: others ->
      cmp_aux others leftmost_expr operator
  | others -> (others, leftmost_expr)

and term tokens =
  let rec term_aux tokens_remaining acc prev_operator =
    let tokens_remaining, expr = factor tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = Minus } as operator) :: others
    | ({ t = Plus } as operator) :: others ->
        term_aux others (Binary (acc, prev_operator, expr)) operator
    | others -> (others, Binary (acc, prev_operator, expr))
  in
  let tokens_remaining, leftmost_expr = factor tokens in
  match consume_newlines tokens_remaining with
  | ({ t = Minus } as operator) :: others | ({ t = Plus } as operator) :: others
    ->
      term_aux others leftmost_expr operator
  | others -> (others, leftmost_expr)

and factor tokens =
  let rec factor_aux tokens_remaining acc prev_operator =
    let tokens_remaining, expr = unary tokens_remaining in
    match consume_newlines tokens_remaining with
    | ({ t = Slash } as operator) :: others
    | ({ t = Star } as operator) :: others
    | ({ t = Percentage } as operator) :: others ->
        factor_aux others (Binary (acc, prev_operator, expr)) operator
    | others -> (others, Binary (acc, prev_operator, expr))
  in
  let tokens_remaining, leftmost_expr = unary tokens in
  match consume_newlines tokens_remaining with
  | ({ t = Slash } as operator) :: others
  | ({ t = Star } as operator) :: others
  | ({ t = Percentage } as operator) :: others ->
      factor_aux others leftmost_expr operator
  | others -> (others, leftmost_expr)

and unary tokens =
  match tokens with
  | { t = Newline } :: others -> unary others
  | ({ t = Bang } as operator) :: others | ({ t = Minus } as operator) :: others
    ->
      let tokens_remaining, expr = unary others in
      (tokens_remaining, Unary (operator, expr))
  | others -> locator others

and locator tokens =
  let rec locator_aux remaining_tokens acc =
    let remaining_tokens, middle_expr =
      maybe_call (consume_newlines remaining_tokens)
    in
    match consume_newlines remaining_tokens with
    | { t = Dot } :: others -> locator_aux others (Locator (acc, middle_expr))
    | others -> (others, Locator (acc, middle_expr))
  in

  let remaining_tokens, leftmost_expr = maybe_call tokens in
  let remaining_tokens = consume_newlines remaining_tokens in
  match remaining_tokens with
  | { t = Dot } :: others -> locator_aux others leftmost_expr
  | others -> (others, leftmost_expr)

and maybe_call tokens =
  let remaining_tokens, expr = primary tokens in
  let remaining_tokens, rexpr =
    match consume_newlines remaining_tokens with
    | { t = LeftParen } :: others ->
        let rec call remaining_tokens prev_expr =
          match consume_newlines remaining_tokens with
          | { t = LeftParen } :: others ->
              let remaining_tokens, expr_list = call_args others in
              call remaining_tokens (Call (prev_expr, expr_list))
          | others -> (remaining_tokens, prev_expr)
        in

        let remaining_tokens, expr_list = call_args others in
        let remaining_tokens, prev_expr =
          (remaining_tokens, Call (expr, expr_list))
        in
        call remaining_tokens prev_expr
    | others -> (remaining_tokens, expr)
  in
  (remaining_tokens, rexpr)

and call_args tokens =
  let rec args_aux remaining_tokens acc =
    match remaining_tokens with
    | { t = Newline } :: others | { t = Comma } :: others -> args_aux others acc
    | { t = RightParen } :: others -> (others, acc)
    | others ->
        let remaining_tokens, expr = expression others in
        args_aux remaining_tokens (expr :: acc)
  in
  let remaining_tokens, exprs = args_aux tokens [] in
  (remaining_tokens, List.rev exprs)

and primary tokens =
  match tokens with
  | { t = Newline } :: others -> primary others
  | { t = LeftParen } :: others -> grouping others
  | { t = LeftBrace } :: others -> maybe_object others
  | { t = True } :: others -> (others, Value (Literal (Bool true)))
  | { t = False } :: others -> (others, Value (Literal (Bool false)))
  | { t = Null } :: others -> (others, Value (Literal Null))
  | { t = Number n } :: others | { t = Plus } :: { t = Number n } :: others ->
      (others, Value (Literal (Num n)))
  | { t = Minus } :: { t = Number n } :: others ->
      (others, Value (Literal (Num (n *. -1.0))))
  | { t = Str s } :: others -> (others, Value (Literal (Str s)))
  | { t = Ident name } :: others -> (others, Value (Variable name))
  | { t = LeftSquareBrace } :: others -> array others
  | { t = New } :: others -> class_instantiate others
  | { t = This } :: others -> (others, This)
  | others -> raise (UnexpectedSequence others)

and class_instantiate tokens =
  let tokens_remaining, expr = maybe_call tokens in
  (tokens_remaining, ClassInst expr)

and array tokens =
  let rec consume_comma = function
    | { t = Newline } :: others -> consume_comma others
    | { t = Comma } :: others -> others
    | others -> others
  in
  let rec array_aux acc remaining_tokens =
    match consume_newlines remaining_tokens with
    | { t = RightSquareBrace } :: others -> (others, ArrayExpr acc)
    | others ->
        let remaining_tokens, expr = expression others in
        let remaining_tokens = consume_comma remaining_tokens in
        array_aux (List.append acc [ expr ]) remaining_tokens
  in
  array_aux [] tokens

and grouping tokens =
  let remaining_tokens, expr = expression tokens in
  match remaining_tokens with
  | { t = RightParen } :: others -> (others, Grouping expr)
  | others -> raise (UnexpectedSequence others)
