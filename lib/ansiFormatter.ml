let build_sequence args =
  List.map string_of_int args |> String.concat ";" |> Printf.sprintf "\x1b[%sm"

let reset = build_sequence [ 0 ]

type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Reset
  | BrightBlack
  | BrightRed
  | BrightGreen
  | BrightYellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | BrightWhite

let base_encode_color = function
  | Black -> 0
  | Red -> 1
  | Green -> 2
  | Yellow -> 3
  | Blue -> 4
  | Magenta -> 5
  | Cyan -> 6
  | White -> 7
  | Default -> 9
  | Reset -> 0
  | BrightBlack -> 60
  | BrightRed -> 61
  | BrightGreen -> 62
  | BrightYellow -> 63
  | BrightBlue -> 64
  | BrightMagenta -> 65
  | BrightCyan -> 66
  | BrightWhite -> 67

let fg_color color =
  match color with Reset -> 0 | color -> base_encode_color color + 30

let bg_color color =
  match color with Reset -> 0 | color -> base_encode_color color + 10

type text_format =
  | Bold
  | Dim
  | Italic
  | Underline
  | Blinking
  | Inverse
  | Hidden
  | Strikethrough

let text_format = function
  | Bold -> 1
  | Dim -> 2
  | Italic -> 3
  | Underline -> 4
  | Blinking -> 5
  | Inverse -> 7
  | Hidden -> 8
  | Strikethrough -> 9

let print_styled styles text =
  Printf.printf "%s%s%s" (build_sequence styles) text reset

let styled styles text =
  Printf.sprintf "%s%s%s" (build_sequence styles) text reset

let style styles = build_sequence styles
