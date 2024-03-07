open Syntax
(* open Typing *)

let rec gen_declspec = function
  | TsInt -> "int"
  | TsShort -> "short"
  | TsLong -> "long"
  | TsChar -> "char"
  | TsSigned -> "signed"
  | TsUnsigned -> "unsigned"
  | TsFloat -> "float"
  | TsDouble -> "double"
  | TsVoid -> "void"
  | TsStruct id -> (
      "struct "
      ^
      match List.nth (List.rev !Env.program) id with
      | StructDecl name -> name
      | _ -> failwith "gen_declspec")
  | TsStructDef id -> (
      "struct "
      ^
      match List.nth (List.rev !Env.program) id with
      | StructDef (name, l) -> name ^ " {\n" ^ gen_fields l ^ "}"
      | _ -> failwith "gen_declspec")
  | TsUnion id -> (
      "union "
      ^
      match List.nth (List.rev !Env.program) id with
      | UnionDecl name -> name
      | _ -> failwith "gen_declspec")
  | TsUnionDef id -> (
      "union "
      ^
      match List.nth (List.rev !Env.program) id with
      | UnionDef (name, l) -> name ^ " {\n" ^ gen_fields l ^ "}"
      | _ -> failwith "gen_declspec")
  | TsTypedef id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl (name, _) -> name
      | _ -> failwith "gen_declspec")
  | ScsTypedef -> "typedef"
  | ScsExtern -> "extern"
  | ScsStatic -> "static"
  | ScsAuto -> "auto"
  | ScsRegister -> "register"
  | TqConst -> "const"
  | TqVolatile -> "volatile"
  | FsInline -> "inline"
  | FsNoreturn -> "_Noreturn"

and gen_declspecs l = String.concat " " (List.map (fun ds -> gen_declspec ds) l)

and gen_decl str = function
  | TConstPtr ((TArr _ | TFun _) as ty) ->
      gen_decl ("(" ^ "*const " ^ str ^ ")") ty
  | TConstPtr ty -> gen_decl ("*const " ^ str) ty
  | TPtr ((TArr _ | TFun _) as ty) -> gen_decl ("(" ^ "*" ^ str ^ ")") ty
  | TPtr ty -> gen_decl ("*" ^ str) ty
  | TArr (ty, _) -> gen_decl (str ^ "[" ^ "]") ty
  | TFun (ty, l) -> gen_decl (str ^ "(" ^ gen_params l ^ ")") ty
  | TDeclSpec l -> gen_declspecs l ^ " " ^ str

and gen_params l =
  String.concat ", " (List.map (fun (name, ty) -> gen_decl name ty) l)

and gen_fields l =
  String.concat ";\n" (List.map (fun (name, ty) -> gen_decl name ty) l) ^ ";\n"
