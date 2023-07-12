open Idk

let run_test prev_tests name test =
  let test_num = match Hashtbl.find_opt prev_tests name with
  | Some prev_num -> Hashtbl.replace prev_tests name (prev_num+1); prev_num+1
  | None -> Hashtbl.replace prev_tests name 0; 0 in
  Printf.printf ":: %s%s -> " name (if test_num > 0 then Printf.sprintf "_%d" test_num else "");

  if test () then (
    Printf.printf "success\n";
    true)
  else (
    Printf.printf "failed\n";
    false)

exception TestsFailed of int

let test common_name (tests : (string * (unit -> bool)) list) =
  Printf.printf "Running tests: %s\n\n" common_name;
  let prev_tests = Hashtbl.create 16 in
  let succeeded =
    List.fold_left
      (fun acc (name, test) -> if run_test prev_tests name test then acc + 1 else acc)
      0 tests
  in
  let test_count = List.length tests in
  Printf.printf "\n%d/%d tests succeeded\n" succeeded test_count;
  if succeeded < test_count then raise (TestsFailed (test_count - succeeded)) else 
  ()

let token_eq a b = String.equal (Token.string_of_tokentype a) (Token.string_of_tokentype b)

let tokenised_eq s t = List.equal token_eq (Lexer.tokenise s) t

let () = test "Lexer" [
  ("numbers", fun _ -> tokenised_eq "23" [Number(23.)]);
  ("numbers", fun _ -> tokenised_eq "23e10" [Number(23e10)]);
  ("numbers", fun _ -> tokenised_eq "23.34" [Number(23.34)]);
  ("numbers", fun _ -> tokenised_eq  "23e-2" [Number(23e-2)]);
  ("numbers", fun _ -> tokenised_eq  "123.0456789" [Number(123.0456789)]);
  ("numbers", fun _ -> tokenised_eq  "1.2e2" [Number(1.2e2)]);

  ("numbers", fun _ -> tokenised_eq "-23" [Minus; Number(23.)]);
  ("numbers", fun _ -> tokenised_eq "-23e10" [Minus; Number(23e10)]);
  ("numbers", fun _ -> tokenised_eq "-23.34" [Minus; Number(23.34)]);
  ("numbers", fun _ -> tokenised_eq  "-23e-2" [Minus; Number(0.23)]);
  ("numbers", fun _ -> tokenised_eq  "-123.0456789" [Minus; Number(123.0456789)]);
  ("numbers", fun _ -> tokenised_eq  "-1.2e2" [Minus; Number(1.2e2)]);
  
  ("single line comment", fun _ -> tokenised_eq  "//**hides in the comment again**" []);
  ("single line comment", fun _ -> tokenised_eq  "//*hides in the comment*\n;" [Semicolon]);

  ("string", fun _ -> tokenised_eq {|"string"|} [Str("string")] );
  ("string", fun _ -> tokenised_eq {|"\tst\rring\n\0"|} [Str("\tst\rring\n\0")] );
  ("string", fun _ -> tokenised_eq {|'\tst\rring\n\0'|} [Str("\tst\rring\n\0")] );
  ("string", fun _ -> tokenised_eq "\"st\nri\ng\"" [Str("st\nri\ng")] );

  ("identifier", fun _ -> tokenised_eq "javascript > rust" [Ident("javascript"); Greater; Ident("rust")]);

]
