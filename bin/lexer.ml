open Token
open Ast

exception UnexpectedCharacter of char
exception UnexpectedEof
exception Unimplemented of string
exception ParsedVal

let tokenise (s : string) : tokentype list =
  let line = ref 0 in
  let pos = ref 0 in
  let tokens = ref [] in

  let add_token t =
    incr pos;
    tokens := !tokens @ [ t ]
  in

  let match_next x =
    if !pos < String.length s - 1 then
      if s.[!pos + 1] == x then (
        incr pos;
        true)
      else false
    else false
  in

  let is_at_end pos = !pos >= String.length s - 1 in

  let parse_string (delim : char) =
    let sbuf = Buffer.create 16 in

    let rec parse_string_aux len chars =
      match chars with
      | [] -> raise UnexpectedEof
      | '\\' :: 'n' :: others ->
          Buffer.add_char sbuf '\n';
          parse_string_aux (len + 2) others
      | '\\' :: '"' :: others ->
          Buffer.add_char sbuf '"';
          parse_string_aux (len + 2) others
      | '\\' :: '\'' :: others ->
          Buffer.add_char sbuf '\'';
          parse_string_aux (len + 2) others
      | '\\' :: 'r' :: others ->
          Buffer.add_char sbuf '\r';
          parse_string_aux (len + 2) others
      | '\\' :: 't' :: others ->
          Buffer.add_char sbuf '\t';
          parse_string_aux (len + 2) others
      | '\\' :: '\\' :: others ->
          Buffer.add_char sbuf '\\';
          parse_string_aux (len + 2) others
      | c :: others ->
          if Char.equal c delim then (len, Buffer.contents sbuf)
          else (
            Buffer.add_char sbuf c;
            parse_string_aux (len + 1) others)
    in

    let explode_string s = List.init (String.length s) (String.get s) in
    let ssub = String.sub s (!pos + 1) (String.length s - !pos - 1) in
    let len, r = parse_string_aux 0 (explode_string ssub) in
    pos := !pos + len + 1;
    r
  in

  try
    while !pos < String.length s do
      match s.[!pos] with
      | '(' -> add_token LeftParen
      | ')' -> add_token RightParen
      | '{' -> add_token LeftBrace
      | '}' -> add_token RightBrace
      | '[' -> add_token LeftSquareBrace
      | ']' -> add_token RightSquareBrace
      | ',' -> add_token Comma
      | '.' -> add_token Dot
      | '+' -> add_token (if match_next '=' then PlusEqual else Plus)
      | '*' -> add_token (if match_next '=' then StarEqual else Star)
      | '-' ->
          add_token
            (if match_next '=' then MinusEqual
             else if match_next '>' then Dot
             else Minus)
      | '|' -> if match_next '|' then add_token Or else add_token Dot
      | '&' -> if match_next '&' then add_token And else ()
      | '%' ->
          add_token (if match_next '=' then PercentageEqual else Percentage)
      | '!' -> add_token (if match_next '=' then BangEqual else Bang)
      | '=' -> add_token (if match_next '=' then EqualEqual else Equal)
      | '<' -> add_token (if match_next '=' then LessEqual else Less)
      | '>' -> add_token (if match_next '=' then GreaterEqual else Greater)
      | '/' -> (
          if !pos >= String.length s - 1 then raise UnexpectedEof
          else
            match s.[!pos + 1] with
            | '/' ->
                while s.[!pos] != '\n' do
                  incr pos
                done;
                incr pos;
                incr line
            | '=' ->
                incr pos;
                add_token SlashEqual
            | '*' -> raise (Unimplemented "Too lazy rn")
            | _ -> add_token Slash)
      | ';' -> add_token Semicolon
      | ':' -> add_token Colon
      | '\n' ->
          incr line;
          add_token Newline
      | '\r' | '\t' | ' ' -> incr pos
      | '"' -> add_token (Str (parse_string '"'))
      | '\'' -> add_token (Str (parse_string '\''))
      | '0' .. '9' ->
          (let len = ref 0 in
           let parse_num_sequence curr_len =
             while
               (match s.[!pos] with '0' .. '9' -> true | _ -> false)
               && not (is_at_end pos)
             do
               incr pos;
               incr curr_len
             done
           in
           try
             parse_num_sequence len;

             if
               is_at_end pos
               || (s.[!pos] != '.' && s.[!pos] != 'e')
               || s.[!pos] == '.'
                  && match s.[!pos + 1] with '0' .. '9' -> false | _ -> true
             then raise ParsedVal
             else incr pos;
             incr len;
             let parsed_e = s.[!pos - 1] == 'e' in

             if parsed_e && s.[!pos] == '-' then (
               incr pos;
               incr len)
             else ();

             parse_num_sequence len;

             if is_at_end pos || s.[!pos] != 'e' || parsed_e then
               raise ParsedVal
             else incr pos;
             incr len;
             if s.[!pos] == '-' then (
               incr pos;
               incr len)
             else ();

             parse_num_sequence len;

             raise ParsedVal
           with ParsedVal ->
             let snum = String.sub s (!pos - !len) !len in
             add_token (Number (float_of_string snum)));
          decr pos (*idk why this is necessary*)
      | 'a' .. 'z' | 'A' .. 'Z' | '_' -> (
          let rec aux len =
            if
              (match s.[!pos] with
              | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> false
              | _ -> true)
              || is_at_end pos
            then len
            else (
              incr pos;
              aux (len + 1))
          in
          let len = aux 0 in
          let sub_s = String.sub s (!pos - len) len in
          decr pos;
          match sub_s with
          | "fun" -> add_token Fun
          | "this" -> add_token This
          | "if" -> add_token If
          | "else" -> add_token Else
          | "while" -> add_token While
          | "do" -> add_token Do
          | "for" -> add_token For
          (* | "print" -> add_token Print // uses external function calls for I/O *)
          | "true" -> add_token True
          | "false" -> add_token False
          | "null" -> add_token Null
          | "return" -> add_token Return
          | "let" -> add_token Let
          | "then" -> add_token Then
          | "break" -> add_token Break
          | _ as sub_s -> add_token (Ident sub_s))
      | c -> raise (UnexpectedCharacter c)
    done;
    raise End_of_file
  with End_of_file -> !tokens
