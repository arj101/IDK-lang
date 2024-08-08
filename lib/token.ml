type tokentype =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftSquareBrace
  | RightSquareBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Colon
  | Slash
  | Star
  | Percentage
  | PlusEqual
  | MinusEqual
  | StarEqual
  | SlashEqual
  | PercentageEqual
  | StarStar
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Ident of string
  | Str of string
  | Number of float
  | Fun
  | This
  | If
  | Else
  | While
  | Do
  | For
  | Let
  | Print
  | True
  | False
  | Return
  | Extends
  | Null
  | And
  | Or
  | Newline
  | Eof
  | Then
  | Break
  | New
  | Class
  | DotProduct

type span = { line : int; col : int; length : int }
type token = { t : tokentype; span : span }

let string_of_tokentype (t : tokentype) : string =
  match t with
  | LeftParen -> "LeftParen"
  | RightParen -> "RightParen"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftSquareBrace -> "LeftSquareBrace"
  | RightSquareBrace -> "RightSquareBrace"
  | Comma -> "Comma"
  | Dot -> "Dot"
  | Minus -> "Minus"
  | Plus -> "Plus"
  | Semicolon -> "Semicolon"
  | Slash -> "Slash"
  | Star -> "Star"
  | Percentage -> "Percentage"
  | PlusEqual -> "PlusEqual"
  | MinusEqual -> "MinusEqual"
  | StarEqual -> "StarEqual"
  | SlashEqual -> "SlashEqual"
  | PercentageEqual -> "PercentageEqual"
  | StarStar -> "StarStar"
  | Bang -> "Bang"
  | BangEqual -> "BangEqual"
  | Equal -> "Equal"
  | EqualEqual -> "EqualEqual"
  | Greater -> "Greater"
  | GreaterEqual -> "GreaterEqual"
  | Less -> "Less"
  | LessEqual -> "LessEqual"
  | Ident name -> "Ident( " ^ name ^ " )"
  | Str s -> "\"" ^ s ^ "\""
  | Number n -> string_of_float n
  | Fun -> "fun"
  | This -> "this"
  | If -> "if"
  | Else -> "else"
  | While -> "while"
  | Do -> "do"
  | For -> "for"
  | Let -> "let"
  | Class -> "class"
  | New -> "new"
  | Print -> "print"
  | True -> "true"
  | False -> "false"
  | Return -> "return"
  | Extends -> "extends"
  | Null -> "null"
  | And -> "&&"
  | Or -> "||"
  | Newline -> "\\n"
  | Eof -> "eof"
  | Then -> "then"
  | Break -> "break"
  | Colon -> ":"
  | DotProduct -> "Dotp"

let token_type_length t =
  match t with
  | Ident name -> String.length name
  | Str s -> String.length s + 2
  | Number n ->
      string_of_float n |> String.length (*FIXME: figure out the actual length*)
  | Fun | This | If | Else | While | Do | For | Let | Class | New | Print | True
  | False | Return | Extends | Null | And | Or | Then | Break ->
      string_of_tokentype t |> String.length
  | GreaterEqual | PlusEqual | LessEqual | EqualEqual | PercentageEqual
  | StarStar | BangEqual | SlashEqual ->
      2
  | t -> 1

let token_length { t; _ } = token_type_length t
let string_of_token { t; _ } = string_of_tokentype t

let string_of_token_list tokens =
  String.concat ", " (List.map string_of_token tokens)
