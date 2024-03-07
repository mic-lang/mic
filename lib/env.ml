open Syntax

exception EnvError of string

let raise exn =
  match exn with
  (*| EnvError msg -> Printf.printf "%s\n" msg;raise exn*)
  | _ -> raise exn

let program : program ref = ref []
let spr fmt s = Printf.sprintf fmt s
let curr_scope : id list ref = ref []
let stack : id list list ref = ref []

let map_to_program l =
  List.rev (List.map (fun n -> (n, List.nth (List.rev !program) n)) l)

let get_scope () = map_to_program !curr_scope
let get_stack () = map_to_program (List.flatten (!curr_scope :: !stack))

let push_def def =
  let id = List.length !program in
  curr_scope := id :: !curr_scope;
  program := def :: !program;
  id

let enter_scope () =
  stack := !curr_scope :: !stack;
  curr_scope := []

let leave_scope () =
  curr_scope := List.hd !stack;
  stack := List.tl !stack

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

let is_decl name = function Decl (n, _) when n = name -> true | _ -> false
let lookup_decl name l = find_item (is_decl name) l

let is_vardef name = function
  | VarDef ((n, _), _) when n = name -> true
  | _ -> false

let lookup_vardef name l = find_item (is_vardef name) l

let is_functiondef name = function
  | FunctionDef ((n, _), _) when n = name -> true
  | _ -> false

let lookup_functiondef name l = find_item (is_functiondef name) l

let lookup_typedef name =
  match lookup_decl name (get_stack ()) with
  | Some id -> (
      print_endline name;
      match List.nth (List.rev !program) id with
      | Decl (_, ty) ->
          let dsl = get_declspec ty in
          if List.mem ScsTypedef dsl then (
            print_endline name;
            Some id)
          else None
      | _ -> None)
  | None -> None

let lookup_var name =
  match lookup_decl name (get_stack ()) with
  | Some id ->
      if Option.is_some (lookup_typedef name) then raise Not_found else id
  | None -> (
      match lookup_vardef name (get_stack ()) with
      | Some id -> id
      | None -> (
          match lookup_functiondef name (get_stack ()) with
          | Some id -> id
          | None ->
              print_endline name;
              raise Not_found))
