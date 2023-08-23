open Env
open Eval
open Exceptions

let call_fn f args = Call (Value f, args)

module CoreArray = struct
  let length _ = function
    | Array elements :: _ ->
        Literal (Num (float_of_int (Array.length !elements)))
    | _ -> assert false

  let push _ = function
    | Array elements :: elt :: _ ->
        elements := Array.append !elements [| elt |];
        Literal Null
    | [ Array elements ] ->
        elements := Array.append !elements [| Literal Null |];
        Literal Null
    | _ -> assert false

  let unshift _ = function
    | Array elements :: elt :: _ ->
        elements := Array.append [| elt |] !elements;
        Literal Null
    | [ Array elements ] ->
        elements := Array.append [| Literal Null |] !elements;
        Literal Null
    | _ -> assert false

  let pop _ = function
    | Array elements :: _ ->
        let prev_len = Array.length !elements in
        let new_len = if prev_len > 0 then prev_len - 1 else prev_len in
        let last_element =
          if prev_len > 0 then Array.get !elements (Array.length !elements - 1)
          else Literal Null
        in
        elements := Array.sub !elements 0 new_len;
        last_element
    | _ -> assert false

  let shift _ = function
    | Array elements :: _ ->
        let prev_len = Array.length !elements in
        let new_len = if prev_len > 0 then prev_len - 1 else prev_len in
        let first_element =
          if prev_len > 0 then Array.get !elements 0 else Literal Null
        in
        elements := Array.sub !elements 0 new_len;
        first_element
    | _ -> assert false

  let map env = function
    | Array elements :: fn :: _ ->
        let new_elements =
          Array.map
            (fun v -> eval_expr env (Call (Value fn, [ Value v ])))
            !elements
        in
        Array (ref new_elements)
    | _ -> raise TypeError

  let filter env = function
    | Array elements :: fn :: _ ->
        let new_elements =
          Array.of_list
            (List.filter
               (fun v ->
                 to_bool env (eval_expr env (Call (Value fn, [ Value v ]))))
               (Array.to_list !elements))
        in
        Array (ref new_elements)
    | _ -> raise TypeError

  let foldl env = function
    | Array elements :: fn :: init :: _ ->
        let new_elements =
          Array.fold_left
            (fun acc v -> eval_expr env (call_fn fn [ Value acc; Value v ]))
            init !elements
        in
        new_elements
    | [ Array elements; fn ] ->
        let new_elements =
          Array.fold_left
            (fun acc v -> eval_expr env (call_fn fn [ Value acc; Value v ]))
            (Literal Null) !elements
        in
        new_elements
    | _ -> raise TypeError

  let foldr env = function
    | Array elements :: fn :: init :: _ ->
        let new_elements =
          Array.fold_right
            (fun v acc -> eval_expr env (call_fn fn [ Value v; Value acc ]))
            !elements init
        in
        new_elements
    | [ Array elements; fn ] ->
        let new_elements =
          Array.fold_right
            (fun acc v -> eval_expr env (call_fn fn [ Value v; Value acc ]))
            !elements (Literal Null)
        in
        new_elements
    | _ -> raise TypeError

  let slice _ = function
    | Array elements :: Literal (Num start) :: Literal (Num end_exc) :: _ ->
        let end_exc =
          if end_exc >= float_of_int (Array.length !elements) then
            float_of_int (Array.length !elements) -. 1.
          else end_exc
        in
        let new_elements =
          Array.sub !elements
            (int_of_float (Float.round start))
            (int_of_float (Float.round (end_exc -. start)))
        in
        Array (ref new_elements)
    | [ Array elements; Literal (Num start) ] ->
        let end_exc = float_of_int (Array.length !elements) in
        let start = if start < 0. then end_exc +. start else start in
        let new_elements =
          Array.sub !elements
            (int_of_float (Float.round start))
            (int_of_float (Float.round (end_exc -. start)))
        in
        Array (ref new_elements)
    | _ -> raise TypeError

  let flat _ args =
    let rec flatten_aux depth a =
      if depth < 0 then a
      else
        let rec flatten_aux_aux a acc subi_start subi_curr =
          if Array.length a > subi_curr then
            match Array.get a subi_curr with
            | Array suba ->
                flatten_aux_aux a
                  (Array.concat
                     [
                       acc;
                       Array.sub a subi_start
                         (Int.max (subi_curr - subi_start - 1) 0);
                       flatten_aux (depth - 1) !suba;
                     ])
                  (subi_curr + 1) (subi_curr + 1)
            | elem -> flatten_aux_aux a acc subi_start (subi_curr + 1)
          else
            Array.concat
              [ acc; Array.sub a subi_start (Array.length a - subi_start) ]
        in
        flatten_aux_aux a [||] 0 0
    in
    match args with
    | Array elements :: Literal (Num depth) :: _ ->
        Array
          (ref (flatten_aux (int_of_float (Float.round depth -. 1.)) !elements))
    | Array elements :: _ -> Array (ref (flatten_aux 0 !elements))
    | _ -> raise TypeError

  let flatmap env args =
    let rec flatmap_aux map_fn a =
      let rec flatmap_aux_aux a acc subi_start subi_curr =
        if Array.length a > subi_curr then
          match
            eval_expr env (call_fn map_fn [ Value (Array.get a subi_curr) ])
          with
          | Array suba ->
              flatmap_aux_aux a
                (Array.concat
                   [
                     acc;
                     Array.sub a subi_start
                       (Int.max (subi_curr - subi_start - 1) 0);
                     !suba;
                   ])
                (subi_curr + 1) (subi_curr + 1)
          | elem ->
              flatmap_aux_aux a
                (Array.append acc [| elem |])
                subi_start (subi_curr + 1)
        else acc
      in
      flatmap_aux_aux a [||] 0 0
    in
    match args with
    | Array elements :: fn :: _ -> Array (ref (flatmap_aux fn !elements))
    | _ -> raise TypeError

  let make _ = function
    | Literal (Num length) :: initial_value :: _ ->
        Array (ref (Array.make (int_of_float length) initial_value))
    | [ Literal (Num length) ] ->
        Array (ref (Array.make (int_of_float length) (Literal Null)))
    | [] -> Array (ref [||])
    | _ -> raise TypeError

  let init env = function
    | Literal (Num length) :: init_fn :: _ ->
        Array
          (ref
             (Array.init (int_of_float length) (fun i ->
                  eval_expr env
                    (call_fn init_fn [ Value (Literal (Num (float_of_int i))) ]))))
    | _ -> raise TypeError

  let foreach env = function
    | Array elements :: fn :: _ ->
        Array.iter
          (fun v -> eval_expr env (call_fn fn [ Value v ]) |> ignore)
          !elements;
        Literal Null
    | _ -> raise TypeError

  let indexof env = function
    | Array elements :: value :: _ ->
        let array_length = Array.length !elements in
        let rec find i =
          if i < array_length then
            if to_bool env (eq env (!elements.(i), value)) then i
            else find (i + 1)
          else -1
        in
        Literal (Num (float_of_int (find 0)))
    | _ -> raise TypeError

  let find env = function
    | Array elements :: finder_fn :: _ -> (
        match
          Array.find_opt
            (fun v ->
              to_bool env (eval_expr env (call_fn finder_fn [ Value v ])))
            !elements
        with
        | Some value -> value
        | None -> Literal Null)
    | _ -> raise TypeError
end

let gen_array_obj parent_env : value =
  let env = Env.create parent_env in
  let array_obj = Object (Some "Array", env) in
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "length" [ "array" ] CoreArray.length;
  def_fn "push" [ "array" ] CoreArray.push;
  def_fn "pop" [ "array" ] CoreArray.pop;
  def_fn "shift" [ "array" ] CoreArray.shift;
  def_fn "unshift" [ "array" ] CoreArray.unshift;

  def_fn "map" [ "array" ] CoreArray.map;
  def_fn "filter" [ "array" ] CoreArray.filter;
  def_fn "foldl" [ "array"; "fn_acc_v"; "init" ] CoreArray.foldl;
  def_fn "foldr" [ "array"; "fn_v_acc"; "init" ] CoreArray.foldr;
  def_fn "slice" [ "array"; "start"; "end_exclusive" ] CoreArray.slice;
  def_fn "flat" [ "array" ] CoreArray.flat;
  def_fn "flat_map" [ "array" ] CoreArray.flatmap;

  def_fn "make" [ "length"; "init" ] CoreArray.make;
  def_fn "init" [ "length"; "init_fn" ] CoreArray.init;

  def_fn "for_each" [ "array"; "iter_fn" ] CoreArray.foreach;

  def_fn "index_of" [ "array"; "value" ] CoreArray.indexof;
  def_fn "find" [ "array"; "finder_fn" ] CoreArray.find;

  array_obj

module Math = struct
  let cos _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.cos n))
    | _ -> raise TypeError

  let sin _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.sin n))
    | _ -> raise TypeError

  let tan _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.tan n))
    | _ -> raise TypeError

  let asin _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.asin n))
    | _ -> raise TypeError

  let acos _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.acos n))
    | _ -> raise TypeError

  let atan _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.atan n))
    | _ -> raise TypeError

  let atan2 _ = function
    | Literal (Num y) :: Literal (Num x) :: others ->
        Literal (Num (Float.atan2 y x))
    | _ -> raise TypeError

  let pow _ = function
    | Literal (Num n) :: Literal (Num p) :: others ->
        Literal (Num (Float.pow n p))
    | _ -> raise TypeError

  let sqrt _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.sqrt n))
    | _ -> raise TypeError

  let ln _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.log n))
    | _ -> raise TypeError

  let log2 _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.log2 n))
    | _ -> raise TypeError

  let log10 _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.log10 n))
    | _ -> raise TypeError

  let exp _ = function
    | Literal (Num n) :: _ -> Literal (Num (Float.exp n))
    | _ -> raise TypeError

  let random _ _ = Literal (Num (Random.float 1.))
end

let gen_math_obj parent_env : value =
  Random.self_init ();
  let env = Env.create parent_env in
  let math_obj = Object (Some "Math", env) in

  let def_num name value = Env.define env name (Literal (Num value)) in
  def_num "PI" Float.pi;
  def_num "E" (Float.exp 1.);
  def_num "MAX_NUM" Float.max_float;
  def_num "MIN_NUM" Float.min_float;

  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in
  def_fn "cos" [ "num" ] Math.cos;
  def_fn "sin" [ "num" ] Math.sin;
  def_fn "tan" [ "num" ] Math.tan;
  def_fn "asin" [ "num" ] Math.asin;
  def_fn "acos" [ "num" ] Math.acos;
  def_fn "atan" [ "num" ] Math.atan;
  def_fn "atan2" [ "x"; "y" ] Math.atan2;
  def_fn "pow" [ "num"; "pow" ] Math.pow;
  def_fn "sqrt" [ "num" ] Math.sqrt;
  def_fn "ln" [ "num" ] Math.ln;
  def_fn "log2" [ "num" ] Math.log2;
  def_fn "log10" [ "num" ] Math.log10;
  def_fn "exp" [ "num" ] Math.exp;
  def_fn "random" [] Math.random;
  math_obj

module CoreString = struct
  let repeat _ = function
    | Literal (Str s) :: Literal (Num n) :: _ ->
        let n = int_of_float n in
        let buf = Buffer.create (String.length s * n) in
        let rec repeat n =
          if n > 0 then (
            Buffer.add_string buf s;
            repeat (n - 1))
          else ()
        in
        repeat n;
        Literal (Str (Buffer.contents buf))
    | _ -> raise TypeError

  let trim _ = function
    | Literal (Str s) :: _ -> Literal (Str (String.trim s))
    | _ -> raise TypeError

  let split _ = function
    | Literal (Str s) :: Literal (Str c) :: _ ->
        assert (String.length c <= 1);
        let c = String.get c 0 in
        let strings =
          String.split_on_char c s |> Array.of_list
          |> Array.map (fun s -> Literal (Str s))
        in
        Array (ref strings)
    | _ -> raise TypeError
end

let gen_string_obj parent_env : value =
  Random.self_init ();
  let env = Env.create parent_env in
  let string_obj = Object (Some "String", env) in

  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "repeat" [ "string"; "n" ] CoreString.repeat;
  def_fn "split" [ "string"; "char"] CoreString.split;

  string_obj
