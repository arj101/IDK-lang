open Env
open Eval
open Exceptions

let array_length _ = function
  | Array elements :: _ -> Literal (Num (float_of_int (Array.length !elements)))
  | _ -> assert false

let call_fn f args = Call (Value f, args)

let array_push _ = function
  | Array elements :: elt :: _ ->
      elements := Array.append !elements [| elt |];
      Literal Null
  | [ Array elements ] ->
      elements := Array.append !elements [| Literal Null |];
      Literal Null
  | _ -> assert false

let array_unshift _ = function
  | Array elements :: elt :: _ ->
      elements := Array.append [| elt |] !elements;
      Literal Null
  | [ Array elements ] ->
      elements := Array.append [| Literal Null |] !elements;
      Literal Null
  | _ -> assert false

let array_pop _ = function
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

let array_shift _ = function
  | Array elements :: _ ->
      let prev_len = Array.length !elements in
      let new_len = if prev_len > 0 then prev_len - 1 else prev_len in
      let first_element =
        if prev_len > 0 then Array.get !elements 0 else Literal Null
      in
      elements := Array.sub !elements 0 new_len;
      first_element
  | _ -> assert false

let array_map env = function
  | Array elements :: fn :: _ ->
      let new_elements =
        Array.map
          (fun v -> eval_expr env (Call (Value fn, [ Value v ])))
          !elements
      in
      Array (ref new_elements)
  | _ -> raise TypeError

let array_filter env = function
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

let array_foldl env = function
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

let array_foldr env = function
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

let array_slice _ = function
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

let array_flat _ args =
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

let array_flatmap env args =
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

let array_make _ = function
  | Literal (Num length) :: initial_value :: _ ->
      Array (ref (Array.make (int_of_float length) initial_value))
  | [ Literal (Num length) ] ->
      Array (ref (Array.make (int_of_float length) (Literal Null)))
  | [] -> Array (ref [||])
  | _ -> raise TypeError

let array_init env = function
  | Literal (Num length) :: init_fn :: _ ->
      Array
        (ref
           (Array.init (int_of_float length) (fun i ->
                eval_expr env
                  (call_fn init_fn [ Value (Literal (Num (float_of_int i))) ]))))
  | _ -> raise TypeError

let gen_array_obj parent_env : value =
  let env = Env.create parent_env in
  let array_obj = Object (Some "Array", env) in
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "length" [ "array" ] array_length;
  def_fn "push" [ "array" ] array_push;
  def_fn "pop" [ "array" ] array_pop;
  def_fn "shift" [ "array" ] array_shift;
  def_fn "unshift" [ "array" ] array_unshift;

  def_fn "map" [ "array" ] array_map;
  def_fn "filter" [ "array" ] array_filter;
  def_fn "foldl" [ "array"; "fn_acc_v"; "init" ] array_foldl;
  def_fn "foldr" [ "array"; "fn_v_acc"; "init" ] array_foldr;
  def_fn "slice" [ "array"; "start"; "end_exclusive" ] array_slice;
  def_fn "flat" [ "array" ] array_flat;
  def_fn "flat_map" [ "array" ] array_flatmap;

  def_fn "make" [ "length"; "init" ] array_make;
  def_fn "init" [ "length"; "init_fn" ] array_init;

  array_obj
