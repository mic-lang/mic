open Syntax

exception EnvError of string

let raise exn =
  match exn with
  (*| EnvError msg -> Printf.printf "%s\n" msg;raise exn*)
  | _ -> raise exn

let program : program ref = ref []
let spr fmt s = Printf.sprintf fmt s
let lparams : id list ref = ref []
let curr_scope : id list ref = ref []
let stack : id list list ref = ref []

let map_to_program l =
  List.rev (List.map (fun n -> (n, List.nth (List.rev !program) n)) l)

let get_scope () = map_to_program !curr_scope
let get_stack () = map_to_program (List.flatten (!curr_scope :: !stack))

let push_lparam lparam =
  let id = List.length !program in
  lparams := id :: !lparams;
  program := lparam :: !program

let curr_depth : int ref = ref 0
let get_curr_depth () = !curr_depth

let push_lparam_depth lparam =
  let id = List.length !program in
  lparams := id :: !lparams;
  program := lparam :: !program;
  let ret = get_curr_depth () in
  incr curr_depth;
  ret

let push_def def =
  let id = List.length !program in
  curr_scope := id :: !curr_scope;
  program := def :: !program;
  id

let enter_scope () =
  stack := !curr_scope :: !stack;
  curr_scope := [];
  incr curr_depth

let leave_scope () =
  curr_scope := List.hd !stack;
  stack := List.tl !stack;
  decr curr_depth

let enter_scope_first () =
  stack := !curr_scope :: !stack;
  curr_scope := !lparams

let leave_scope_last () =
  curr_scope := List.hd !stack;
  stack := List.tl !stack;
  curr_depth := 0

let update_program id def =
  program :=
    List.rev
      (List.mapi (fun i x -> if i = id then def else x) (List.rev !program))

let is_structdecl name = function
  | StructDecl n when n = name -> true
  | _ -> false

let is_uniondecl name = function
  | UnionDecl n when n = name -> true
  | _ -> false

let is_structdef name = function
  | StructDef (n, _) when n = name -> true
  | _ -> false

let is_uniondef name = function
  | UnionDef (n, _) when n = name -> true
  | _ -> false

let rec find_item p = function
  | (id, item) :: _ when p item -> Some id
  | _ :: xs -> find_item p xs
  | [] -> None

let lookup_structdecl name l = find_item (is_structdecl name) l
let lookup_uniondecl name l = find_item (is_uniondecl name) l
let lookup_structdef name l = find_item (is_structdef name) l
let lookup_uniondef name l = find_item (is_uniondef name) l

let make_structdecl name =
  match lookup_structdecl name (get_stack ()) with
  | Some id -> TsStruct id
  | None -> (
      match lookup_structdef name (get_stack ()) with
      | Some id -> TsStruct id
      | None -> TsStruct (push_def (StructDecl name)))

let make_uniondecl name =
  match lookup_uniondecl name (get_stack ()) with
  | Some id -> TsUnion id
  | None -> TsUnion (push_def (UnionDecl name))

let make_structdef name decl =
  match lookup_structdecl name (get_scope ()) with
  | Some id ->
      update_program id (StructDef (name, decl));
      TsStructDef id
  | None -> (
      match lookup_structdef name (get_scope ()) with
      | Some _ -> failwith "redifinition of struct"
      | None -> TsStructDef (push_def (StructDef (name, decl))))

let make_uniondef name decl =
  match lookup_uniondecl name (get_scope ()) with
  | Some id ->
      update_program id (UnionDef (name, decl));
      TsUnionDef id
  | None -> (
      match lookup_uniondef name (get_scope ()) with
      | Some _ -> failwith "redifinition of struct"
      | None -> TsUnionDef (push_def (UnionDef (name, decl))))

let is_decl name = function
  | (Decl (n, _) | GDecl (n, _) | LDecl (_, _, (n, _))) when n = name -> true
  | _ -> false

let lookup_decl name l = find_item (is_decl name) l
let lookup_decl name = lookup_decl name (get_stack ())

let is_vardef name = function
  | (VarDef ((n, _), _) | GVarDef ((n, _), _)) when n = name -> true
  | _ -> false

let lookup_vardef name l = find_item (is_vardef name) l
let lookup_vardef name = lookup_vardef name (get_stack ())

let is_functiondef name = function
  | FunctionDef ((n, _), _) when n = name -> true
  | LFunctionDef (_, _, (n, _), _) when n = name -> true
  | _ -> false

let lookup_functiondef name l = find_item (is_functiondef name) l
let lookup_functiondef name = lookup_functiondef name (get_stack ())

let is_nonlid_functiondef name = function
  | FunctionDef ((n, _), _) when n = name -> true
  | _ -> false

let lookup_nonlid_functiondef name l = find_item (is_nonlid_functiondef name) l

let lookup_nonlid_functiondef name =
  lookup_nonlid_functiondef name (get_stack ())

let is_lid name = function
  | LFunctionDef (_, _, (n, _), _) when n = name -> true
  | _ -> false

let lookup_lid name l = find_item (is_lid name) l
let lookup_lid name = lookup_lid name (get_stack ())
let is_depth name = function Block (n, _) when n = name -> true | _ -> false
let lookup_depth name l = find_item (is_depth name) l
let lookup_depth name = lookup_depth name (get_stack ())
let is_kind name = function Kind n when n = name -> true | _ -> false
let lookup_kind name l = find_item (is_kind name) l
let lookup_kind name = lookup_kind name (get_stack ())

let get_depth name =
  match lookup_depth name with
  | Some id -> (
      match List.nth (List.rev !program) id with
      | Block (name, depth) -> Depth (name, depth)
      | _ -> failwith "get_depth")
  | None -> failwith "get_depth"

let lookup_nontypedef_decl name =
  match lookup_decl name with
  | Some id -> (
      match List.nth (List.rev !program) id with
      | Decl (_, ty) | GDecl (_, ty) ->
          let dsl = get_declspec ty in
          if not (List.mem ScsTypedef dsl) then Some id else None
      | _ -> None)
  | None -> None

let lookup_typedef name =
  match lookup_decl name with
  | Some id -> (
      match List.nth (List.rev !program) id with
      | Decl (_, ty) | GDecl (_, ty) ->
          let dsl = get_declspec ty in
          if List.mem ScsTypedef dsl then Some id else None
      | _ -> None)
  | None -> None

type id_kind = IdUsual | IdLifetime | IdType | IdBlock | IdKind

let lookup_id_kind name =
  let lookup_func =
    [
      (lookup_depth, IdBlock);
      (lookup_kind, IdKind);
      (lookup_nontypedef_decl, IdUsual);
      (lookup_vardef, IdUsual);
      (lookup_typedef, IdType);
      (lookup_nonlid_functiondef, IdUsual);
      (lookup_lid, IdLifetime);
    ]
  in
  let dic =
    List.filter_map
      (function Some id, kind -> Some (id, kind) | _ -> None)
      (List.map (fun (f, kind) -> (f name, kind)) lookup_func)
  in
  match List.fast_sort (fun (x, _) (y, _) -> -compare x y) dic with
  | [] -> failwith "lookup_id_kind"
  | x :: _ -> x

let lookup_var name =
  match lookup_id_kind name with
  | id, IdUsual | id, IdLifetime -> id
  | _ -> failwith "lookup_var"
