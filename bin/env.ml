open Token

exception MaxScopeDepthExceeded
exception PopOutOfRootScope

type expr =
  | If of expr * expr * expr option
  | While of expr * expr
  | For of expr * expr * expr * expr
  | Binary of expr * tokentype * expr
  | Unary of tokentype * expr
  | Block of expr list
  | Grouping of expr
  | Call of expr * expr list
  | Value of value
  | BlockReturn of expr
  | StmtExpr of stmt
  | Locator of expr * expr
  | ObjectExpr of (ident, expr) Hashtbl.t
  | ArrayExpr of expr list
  | This

and stmt =
  | Return of expr
  | Break of expr
  | Print of expr
  | Decl of ident * expr
  | LocatorAssign of expr * expr
  | Assign of ident * expr

and value =
  | Fun of ident option * ident list * expr
  | ClosureFun of env * ident option * ident list * expr
  | ExtFun of ident * ident list * (env -> value list -> value)
  | Variable of ident
  | Object of env
  | Array of value array ref
  | Literal of literal

and literal = Num of float | Str of string | Bool of bool | Null
and ident = string

and env = {
  mutable scope : (string, value) Hashtbl.t;
  parent : env option;
  mutable this_ref : env option;
}

(*Creates an environment that has no parent, but a 'this' reference*)
(*Useful for using evaluating object fileds like any ohter expression*)
(*Because with an ordinary env, expression evaluation can use values from parent environments*)
let create_object_wrapper env =
  { scope = env.scope; parent = None; this_ref = Some env }

let create parent =
  let env_created = { scope = Hashtbl.create 16; parent; this_ref = None } in
  env_created.this_ref <-
    (match parent with
    | Some parent -> parent.this_ref
    | None -> Some env_created);
  env_created

let define env name value = Hashtbl.replace env.scope name value

exception VarNotFound of ident

let rec update env name value =
  match Hashtbl.find_opt env.scope name with
  | Some _ -> Hashtbl.replace env.scope name value
  | None -> (
      match env.parent with
      | Some p_env -> update p_env name value
      | None -> raise (VarNotFound name))

let rec get env name =
  match Hashtbl.find_opt env.scope name with
  | Some value -> value
  | None -> (
      match env.parent with
      | Some p_env -> get p_env name
      | None -> raise (VarNotFound name))

let get_field env name =
  match Hashtbl.find_opt env.scope name with
  | Some value -> value
  | None -> raise (VarNotFound name)

let set_field env name value = Hashtbl.replace env.scope name value
