open Token
open Ast
open AnsiFormatter

exception UnexpectedCharacter of char
exception UnexpectedEof

let rec consume_single_line_comment = function
  | '\n' :: others -> others
  | [] -> []
  | c :: others -> consume_single_line_comment others

let rec consume_multiline_comment = function
  | '*' :: '/' :: others -> others
  | '/' :: '*' :: others ->
      consume_multiline_comment others
      |> consume_multiline_comment (*nested comments*)
  | [] -> []
  | c :: others -> consume_multiline_comment others

type num_format = Hex | Octal

let parse_hex_digit c =
  match Char.lowercase_ascii c with
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | c -> raise (UnexpectedCharacter c)

let parse_octal_digit = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | c -> raise (UnexpectedCharacter c)

let parse_hex chars =
  let rec aux num chars len =
    match chars with
    | ('0' .. '9' as c) :: others
    | ('a' .. 'z' as c) :: others
    | ('A' .. 'Z' as c) :: others ->
        aux ((num lsl 4) + parse_hex_digit c) others (len + 1)
    | others -> (num, len, others)
  in
  aux 0 chars 0

let parse_octal chars =
  let rec aux num chars len =
    match chars with
    | ('0' .. '7' as c) :: others ->
        aux ((num lsl 3) + parse_octal_digit c) others (len + 1)
    | others -> (num, len, others)
  in
  aux 0 chars 0

let parse_number chars =
  let num_chars = Buffer.create 16 in

  let rec sign = function
    | '-' :: others -> (-1, others)
    | others -> (1, others)
  in

  let rec sign_char = function
    | ('-' as s) :: others | ('+' as s) :: others ->
        Buffer.add_char num_chars s;
        others
    | others -> others
  in

  let rec non_base10 = function
    | '0' :: 'x' :: others -> Ok (Hex, others)
    | '0' :: 'o' :: others -> Ok (Octal, others)
    | others -> Error others
  in

  let rec integer = function
    | ('0' .. '9' as n) :: others ->
        Buffer.add_char num_chars n;
        integer others
    | others -> others
  in
  let decimal = function
    | '.' :: others ->
        Buffer.add_char num_chars '.';
        integer others
    | others -> others
  in
  let exponent = function
    | 'E' :: others | 'e' :: others ->
        Buffer.add_char num_chars 'E';
        let others = sign_char others in
        integer others
    | others -> others
  in

  let sign, chars = sign chars in

  non_base10 chars
  |> Result.map_error (fun chars ->
         let chars = integer chars |> decimal |> exponent in

         ( Number
             (float_of_int sign *. float_of_string (Buffer.contents num_chars)),
           (Buffer.length num_chars + if sign < 0 then 1 else 0),
           chars ))
  |> Result.map (fun (base, chars) ->
         match base with
         | Hex ->
             let num, len, chars = parse_hex chars in
             (Number (float_of_int num *. float_of_int sign), len + 2, chars)
         | Octal ->
             let num, len, chars = parse_octal chars in
             (Number (float_of_int num *. float_of_int sign), len + 2, chars))
  |> function
  | Ok x -> x
  | Error x -> x

let boundarychars = " \t\n\r(){}[];,.+-*/%&|^!~<>?:=\\\"'"

let parse_word chars =
  let word_chars = Buffer.create 16 in
  let rec word_start = function
    | ('a' .. 'z' as c) :: others
    | ('A' .. 'Z' as c) :: others
    | ('_' as c) :: others
    | c :: others
      when not (String.contains boundarychars c) ->
        Buffer.add_char word_chars c;
        Ok others
    | others -> Error others
  in
  let rec aux = function
    | ('a' .. 'z' as c) :: others
    | ('A' .. 'Z' as c) :: others
    | ('_' as c) :: others
    | ('0' .. '9' as c) :: others
    | c :: others
      when not (String.contains boundarychars c) ->
        Buffer.add_char word_chars c;
        aux others
    | others -> others
  in
  let chars = Result.get_ok (word_start chars) in
  let chars = aux chars in
  (Buffer.contents word_chars, chars)

let parse_string chars delimiter =
  let string_chars = Buffer.create 16 in
  let chars =
    match chars with
    | c :: others when c == delimiter -> others
    | _ -> assert false
  in

  let rec aux = function
    | '\\' :: 'r' :: others ->
        Buffer.add_char string_chars '\r';
        aux others
    | '\\' :: 't' :: others ->
        Buffer.add_char string_chars '\t';
        aux others
    | '\\' :: 'n' :: others ->
        Buffer.add_char string_chars '\n';
        aux others
    | '\\' :: 'x' :: h1 :: h0 :: others ->
        (parse_hex_digit h1 lsl 4) + parse_hex_digit h0
        |> Char.chr
        |> Buffer.add_char string_chars;
        aux others
    | '\\' :: '0' :: o1 :: o0 :: others ->
        (parse_octal_digit o1 lsl 3) + parse_octal_digit o0
        |> Char.chr
        |> Buffer.add_char string_chars;
        aux others
    | '\\' :: '\\' :: others ->
        Buffer.add_char string_chars '\\';
        aux others
    | '\\' :: c :: others when c == delimiter ->
        Buffer.add_char string_chars c;
        aux others
    | c :: others when c == delimiter -> others
    | c :: others ->
        Buffer.add_char string_chars c;
        aux others
    | [] -> raise UnexpectedEof
  in

  let chars = aux chars in
  (Str (Buffer.contents string_chars), Buffer.length string_chars + 2, chars)

let parse_keyword_or_ident chars =
  let word, chars = parse_word chars in
  let token =
    match word with
    | "fun" -> Fun
    | "this" -> This
    | "if" -> If
    | "else" -> Else
    | "while" -> While
    | "do" -> Do
    | "for" -> For
    | "true" -> True
    | "false" -> False
    | "null" -> Null
    | "return" -> Return
    | "let" -> Let
    | "then" -> Then
    | "break" -> Break
    | "new" -> New
    | "class" -> Class
    | "extends" -> Extends
    | "⨀" | "dot" -> DotProduct
    | ident -> Ident ident
  in
  (token, String.length word, chars)

let tokenise (s : string) : token list =
  let chars = String.fold_left (fun chars c -> c :: chars) [] s |> List.rev in
  let rec add_aux others line col tokens token length =
    aux others line (col + length)
      ({ t = token; span = { length; line; col } } :: tokens)
  and aux chars line col tokens =
    let add others token length = add_aux others line col tokens token length in
    match chars with
    | '\n' :: others -> aux others (line + 1) 0 tokens
    | '\r' :: others | ' ' :: others -> aux others line (col + 1) tokens
    | '\t' :: others ->
        "warning: tab character may cause improper formatting in error reports \
         ;)\n"
        |> print_styled [ fg_color Yellow ];
        aux others line (col + 1) tokens
    | '(' :: others -> add others LeftParen 1
    | ')' :: others -> add others RightParen 1
    | '{' :: others -> add others LeftBrace 1
    | '}' :: others -> add others RightBrace 1
    | '[' :: others -> add others LeftSquareBrace 1
    | ']' :: others -> add others RightSquareBrace 1
    | ',' :: others -> add others Comma 1
    | '.' :: others -> add others Dot 1
    | '+' :: '=' :: others -> add others PlusEqual 2
    | '+' :: others -> add others Plus 1
    | '*' :: '=' :: others -> add others StarEqual 2
    | '*' :: others -> add others Star 1
    | '-' :: '=' :: others -> add others MinusEqual 2
    | '-' :: others -> add others Minus 1
    | '|' :: '|' :: others -> add others Or 2
    | '&' :: '&' :: others -> add others And 2
    | '%' :: '=' :: others -> add others PercentageEqual 2
    | '%' :: others -> add others Percentage 1
    | '!' :: '=' :: others -> add others BangEqual 2
    | '!' :: others -> add others Bang 1
    | '=' :: '=' :: others -> add others EqualEqual 2
    | '=' :: others -> add others Equal 1
    | '<' :: '=' :: others -> add others LessEqual 2
    | '<' :: others -> add others Less 1
    | '>' :: '=' :: others -> add others GreaterEqual 2
    | '>' :: others -> add others Greater 1
    | ';' :: others -> add others Semicolon 1
    | ':' :: others -> add others Colon 1
    | (('\'' as delimiter) :: others as chars)
    | (('"' as delimiter) :: others as chars) ->
        let str, length, others = parse_string chars delimiter in
        add others str length
    | '0' .. '9' :: others as chars ->
        let num, num_len, others = parse_number chars in
        add others num num_len
    | (('a' .. 'z' as c) :: others as chars)
    | (('A' .. 'Z' as c) :: others as chars)
    | (('_' as c) :: others as chars)
    | (c :: others as chars)
      when not (String.contains boundarychars c) ->
        let token, length, others = parse_keyword_or_ident chars in
        add others token length
    | c :: others -> raise (UnexpectedCharacter c)
    | [] -> tokens
  in
  List.rev (aux chars 0 0 [])
