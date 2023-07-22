open Env

exception TypeError

let array_length _ = function
  | Array elements :: _ -> Literal (Num (float_of_int (Array.length !elements)))
  | _ -> raise TypeError

let gen_array_obj parent_env : value =
  let env = Env.create parent_env in
  let array_obj = Object (Some "Array", env) in
  let def_fn name params f = Env.define env name (ExtFun (name, params, f)) in

  def_fn "length" [ "array" ] array_length;

  array_obj
