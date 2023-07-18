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

type token = { t_type : tokentype; lexeme : string; line : int }
