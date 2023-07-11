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
  | ClassDecl of ident * expr list
  | ClassInst of expr
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
  | Object of ident option * env
  | Array of value array ref
  | Literal of literal

and literal = Num of float | Str of string | Bool of bool | Null
and ident = string
and virtuals = Class of ident * (string, value) Hashtbl.t

and env = {
  mutable scope : (string, value) Hashtbl.t;
  parent : env option;
  mutable virtuals : (string, virtuals) Hashtbl.t option;
  mutable this_ref : env option;
}

(*Creates an environment that has no parent, but a 'this' reference*)
(*Useful for using evaluating object fileds like any ohter expression*)
(*Because with an ordinary env, expression evaluation can use values from parent environments*)
let create_object_wrapper env =
  { scope = env.scope; virtuals = None; parent = None; this_ref = Some env }

let create_this_ref_wrapper env new_this =
  {
    scope = env.scope;
    virtuals = env.virtuals;
    parent = env.parent;
    this_ref = new_this;
  }

let create parent =
  let env_created =
    { scope = Hashtbl.create 16; virtuals = None; parent; this_ref = None }
  in
  env_created.this_ref <-
    (match parent with
    | Some parent -> parent.this_ref
    | None -> Some env_created);
  env_created

let define env name value = Hashtbl.replace env.scope name value

let define_virtual env name value =
  if Option.is_none env.virtuals then env.virtuals <- Some (Hashtbl.create 2)
  else ();
  match env.virtuals with
  | Some virtuals -> Hashtbl.replace virtuals name value
  | _ -> assert false

exception VarNotFound of ident
exception VirtualNotFound of ident

let rec get_virtual env name =
  match Option.map (fun v -> Hashtbl.find_opt v name) env.virtuals with
  | Some (Some value) -> value
  | _ -> (
      match env.parent with
      | Some env -> get_virtual env name
      | None -> raise (VirtualNotFound name))

let rec update_virtual env name value =
  let search_parent env =
    match env.parent with
    | Some env -> update_virtual env name value
    | None -> raise (VirtualNotFound name)
  in
  match env.virtuals with
  | Some virtuals -> (
      match Hashtbl.find_opt virtuals name with
      | Some _ -> Hashtbl.replace virtuals name value
      | None -> search_parent env)
  | None -> search_parent env

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
