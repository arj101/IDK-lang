open Env

exception TypeError

let array_length _ = function
  | Array elements :: _ -> Literal (Num (float_of_int (Array.length !elements)))
  | _ -> assert false

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
        if prev_len > 0 then Array.get !elements 0
        else Literal Null
      in
      elements := Array.sub !elements 0 new_len;
      first_element
  | _ -> assert false


let gen_array_obj parent_env : value =
  let env = Env.create parent_env in
  let array_obj = Object (Some "Array", env) in
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "length" [ "array" ] array_length;
  def_fn "push" [ "array" ] array_push;
  def_fn "pop" [ "array" ] array_pop;
  def_fn "shift" [ "array" ] array_shift;
  def_fn "unshift" [ "unshift" ] array_unshift;

  array_obj
