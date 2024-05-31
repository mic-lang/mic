open Syntax

exception EnvError of string

let raise exn =
  match exn with
  (*| EnvError msg -> Printf.printf "%s\n" msg;raise exn*)
  | _ -> raise exn

let program : program ref = ref []
let lparams : program ref = ref []
let spr fmt s = Printf.sprintf fmt s
let lscope : id list ref = ref []
let curr_scope : id list ref = ref []
let stack : id list list ref = ref []

type defined_place = Lparam of id | Stack of id [@@deriving show]
type pairs = (defined_place * expr item) list [@@deriving show]

let map_to_program l =
  List.rev (List.map (fun n -> (Stack n, List.nth (List.rev !program) n)) l)

let map_to_lparam l =
  List.rev (List.map (fun n -> (Lparam n, List.nth (List.rev !lparams) n)) l)

let get_scope () = map_to_program !curr_scope
let get_stack () = map_to_program (List.flatten (!curr_scope :: !stack))
let get_lscope () = map_to_lparam !lscope
let decls : expr decl list ref = ref []
let push_decl_in_params decl = decls := decl :: !decls

let push_def def =
  let id = List.length !program in
  curr_scope := id :: !curr_scope;
  program := def :: !program;
  id

let curr_depth : int ref = ref 0
let get_curr_depth () = !curr_depth

let push_def_ lparam =
  ignore (push_def lparam);
  let ret = get_curr_depth () in
  incr curr_depth;
  ret

let push_params name depth =
  List.iter
    (fun decl -> ignore (push_def (Param (decl, Depth (name, depth), ref Has))))
    !decls

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
  curr_scope := [];
  incr curr_depth

let leave_scope_last () =
  curr_scope := List.hd !stack;
  stack := List.tl !stack;
  decr curr_depth;
  curr_depth := 0;
  lscope := [];
  lparams := [];
  let ret = !decls in
  decls := [];
  ret

let update_program id def =
  program :=
    List.rev
      (List.mapi (fun i x -> if i = id then def else x) (List.rev !program))

let is_structdecl name = function
  | StructDecl (n, _) when n = name -> true
  | _ -> false

let is_uniondecl name = function
  | UnionDecl (n, _) when n = name -> true
  | _ -> false

let is_structdef name = function
  | StructDef (n, _, _) when n = name -> true
  | _ -> false

let is_uniondef name = function
  | UnionDef (n, _, _) when n = name -> true
  | _ -> false

let rec find_item p acc = function
  | (id, item) :: xs when p item -> find_item p (id :: acc) xs
  | _ :: xs -> find_item p acc xs
  | [] -> (
      match List.fast_sort (fun x y -> -compare x y) acc with
      | x :: _ -> Some x
      | [] -> None)

let find_item p def = find_item p [] def
let lookup_structdecl name l = find_item (is_structdecl name) l
let lookup_uniondecl name l = find_item (is_uniondecl name) l
let lookup_structdef name l = find_item (is_structdef name) l
let lookup_uniondef name l = find_item (is_uniondef name) l

let make_structdecl name lparams =
  match lookup_structdecl name (get_stack ()) with
  | Some (Stack id) -> TsStruct (id, lparams)
  | _ -> (
      match lookup_structdef name (get_stack ()) with
      | Some (Stack id) -> TsStruct (id, lparams)
      | _ -> TsStruct (push_def (StructDecl (name, lparams)), lparams))

let make_uniondecl name lparams =
  match lookup_uniondecl name (get_stack ()) with
  | Some (Stack id) -> TsUnion (id, lparams)
  | _ -> (
      match lookup_structdef name (get_stack ()) with
      | Some (Stack id) -> TsUnion (id, lparams)
      | _ -> TsUnion (push_def (UnionDecl (name, lparams)), lparams))

let make_structdef name lparams decl =
  match lookup_structdecl name (get_scope ()) with
  | Some (Stack id) ->
      update_program id (StructDef (name, lparams, decl));
      TsStructDef id
  | _ -> (
      match lookup_structdef name (get_scope ()) with
      | Some (Stack id) ->
          update_program id (StructDef (name, lparams, decl));
          TsStructDef id
      | _ -> TsStructDef (push_def (StructDef (name, lparams, decl))))

let make_uniondef name lparams decl =
  match lookup_uniondecl name (get_scope ()) with
  | Some (Stack id) ->
      update_program id (UnionDef (name, lparams, decl));
      TsUnionDef id
  | _ -> (
      match lookup_uniondef name (get_scope ()) with
      | Some (Stack id) ->
          update_program id (UnionDef (name, lparams, decl));
          TsUnionDef id
      | _ -> TsUnionDef (push_def (UnionDef (name, lparams, decl))))

let is_decl name = function
  | Param ((n, _), _, _)
  | Decl ((n, _), _, _)
  | GDecl (n, _)
  | LDecl (_, (n, _))
    when n = name ->
      true
  | _ -> false

let lookup_decl name l = find_item (is_decl name) l
let lookup_decl name = lookup_decl name (get_stack ())

let is_vardef name = function
  | (VarDef ((n, _), _, _, _) | GVarDef ((n, _), _)) when n = name -> true
  | _ -> false

let lookup_vardef name l = find_item (is_vardef name) l
let lookup_vardef name = lookup_vardef name (get_stack ())

let is_functiondef name = function
  | FunctionDef ((n, _), _, _) when n = name -> true
  | LFunctionDef (_, (n, _), _, _) when n = name -> true
  | _ -> false

let lookup_functiondef name l = find_item (is_functiondef name) l
let lookup_functiondef name = lookup_functiondef name (get_stack ())

let is_nonlid_functiondef name = function
  | FunctionDef ((n, _), _, _) when n = name -> true
  | _ -> false

let lookup_nonlid_functiondef name l = find_item (is_nonlid_functiondef name) l

let lookup_nonlid_functiondef name =
  lookup_nonlid_functiondef name (get_stack ())

let is_lid name = function
  | (LFunctionDef (_, (n, _), _, _) | LDecl (_, (n, _))) when n = name -> true
  | _ -> false

let lookup_lid name l = find_item (is_lid name) l
let lookup_lid name = lookup_lid name (get_stack ())
let is_depth name = function Block (n, _) when n = name -> true | _ -> false
let lookup_depth name l = find_item (is_depth name) l
let lookup_depth name = lookup_depth name (get_stack ())

let lookup_last_depth () =
  let rec find_depth = function
    | [] -> Global
    | (_, Block (name, depth)) :: _ -> Depth (name, depth)
    | _ :: xs -> find_depth xs
  in
  find_depth (List.rev (get_stack ()))

let is_kind name = function Kind n when n = name -> true | _ -> false
let lookup_kind name l = find_item (is_kind name) l
let lookup_kind name = lookup_kind name (get_stack ())

let get_depth name =
  match lookup_depth name with
  | Some (Lparam id) -> (
      match List.nth (List.rev !lparams) id with
      | Block (name, depth) -> (name, depth)
      | _ -> failwith "get_depth")
  | Some (Stack id) -> (
      match List.nth (List.rev !program) id with
      | Block (name, depth) -> (name, depth)
      | _ -> failwith "get_depth")
  | _ -> failwith "get_depth"

let lookup_nontypedef_decl name =
  match lookup_decl name with
  | Some (Stack id) -> (
      match List.nth (List.rev !program) id with
      | Param ((_, ty), _, _) | Decl ((_, ty), _, _) | GDecl (_, ty) ->
          let dsl = get_declspec ty in
          if not (List.mem ScsTypedef dsl) then Some (Stack id) else None
      | _ -> None)
  | _ -> None

let lookup_typedef1 name =
  match lookup_decl name with
  | Some (Stack id) -> (
      match List.nth (List.rev !program) id with
      | Decl ((_, ty), _, _) | GDecl (_, ty) ->
          let dsl = get_declspec ty in
          if List.mem ScsTypedef dsl then Some id else None
      | _ -> None)
  | _ -> None

let lookup_typedef2 name =
  match lookup_typedef1 name with Some id -> Some (Stack id) | None -> None

type id_kind = IdUsual | IdLifetime | IdType | IdBlock | IdKind
[@@deriving show]

type pairs_ = (defined_place * id_kind) list [@@deriving show]

let lookup_id_kind name =
  let lookup_func =
    [
      (lookup_depth, IdBlock);
      (lookup_kind, IdKind);
      (lookup_nontypedef_decl, IdUsual);
      (lookup_vardef, IdUsual);
      (lookup_typedef2, IdType);
      (lookup_nonlid_functiondef, IdUsual);
      (lookup_lid, IdLifetime);
    ]
  in
  let dic =
    List.filter_map
      (function Some id, kind -> Some (id, kind) | _ -> None)
      (List.map (fun (f, kind) -> (f name, kind)) lookup_func)
  in
  match
    List.fast_sort
      (fun (x, _) (y, _) ->
        match (x, y) with
        | Stack x, Stack y -> -compare x y
        | Stack _, Lparam _ -> 1
        | Lparam _, Stack _ -> -1
        | Lparam x, Lparam y -> -compare x y)
      dic
  with
  | [] -> failwith ("name not found: " ^ name)
  | x :: _ -> x

let lookup_var name =
  match lookup_id_kind name with
  | Stack id, IdUsual | Stack id, IdLifetime -> id
  | _ -> failwith ("var not found: " ^ name)
