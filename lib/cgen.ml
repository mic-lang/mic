open Syntax

let gen_ident nests str = String.make (nests * 4) ' ' ^ str

let rec gen_declspec nest = function
  | TsBool -> "bool"
  | TsInt -> "int"
  | TsShort -> "short"
  | TsLong -> "long"
  | TsChar -> "char"
  | TsSigned -> "signed"
  | TsUnsigned -> "unsigned"
  | TsFloat -> "float"
  | TsDouble -> "double"
  | TsVoid -> "void"
  | TsStruct (id, _) -> (
      "struct "
      ^
      match List.nth (List.rev !Env.program) id with
      | StructDecl (name, _) -> name
      | StructDef (name, _, _) -> name
      | _ -> failwith "gen_declspec")
  | TsStructDef id -> (
      "struct "
      ^
      match List.nth (List.rev !Env.program) id with
      | StructDef (name, _, l) ->
          name ^ " {\n" ^ gen_fields nest l ^ ("}" |> gen_ident nest)
      | _ -> failwith "gen_declspec")
  | TsUnion (id, _) -> (
      "union "
      ^
      match List.nth (List.rev !Env.program) id with
      | UnionDecl (name, _) -> name
      | UnionDef (name, _, _) -> name
      | _ -> failwith "gen_declspec")
  | TsUnionDef id -> (
      "union "
      ^
      match List.nth (List.rev !Env.program) id with
      | UnionDef (name, _, l) ->
          name ^ " {\n" ^ gen_fields nest l ^ ("}" |> gen_ident nest)
      | _ -> failwith "gen_declspec")
  | TsTypedef id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl ((name, _), _, _) | GDecl (name, _) -> name
      | _ -> failwith "gen_declspec")
  | TsVarlist -> "__builtin_va_list"
  | ScsTypedef -> "typedef"
  | ScsExtern -> "extern"
  | ScsStatic -> "static"
  | ScsAuto -> "auto"
  | ScsRegister -> "register"
  | TqConst -> "const"
  | TqVolatile -> "volatile"
  | FsInline -> "inline"
  | FsNoreturn -> "_Noreturn"

and gen_declspecs nest l =
  String.concat " " (List.map (fun ds -> gen_declspec nest ds) l)

and gen_type_qual = function
  | Const -> "const"
  | Volatile -> "volatile"
  | Drop -> ""

and gen_type_quals l =
  let str = String.concat " " (List.map (fun tq -> gen_type_qual tq) l) in
  if str = "" then "" else str ^ " "

and gen_decl nest str = function
  | TVar { var_ty = ty; _ } -> gen_decl nest str ty
  | TPtr { pointee_ty = (TArr _ | TFun _) as ty; pointee_qual = quals; _ } ->
      gen_decl nest ("(" ^ "*" ^ gen_type_quals quals ^ str ^ ")") ty
  | TPtr { pointee_ty = ty; pointee_qual = quals; _ } ->
      gen_decl nest ("*" ^ gen_type_quals quals ^ str) ty
  | TArr (ty, expr) -> gen_decl nest (str ^ "[" ^ gen_expr expr ^ "]") ty
  | TFun (ty, l) -> gen_decl nest (str ^ "(" ^ gen_params nest l ^ ")") ty
  | TDeclSpec l -> gen_declspecs nest l ^ if str = "" then "" else " " ^ str
  | TBlock (Depth (name, _)) -> "mi_heap_t* " ^ name
  | TBlock _ -> "mi_heap_t*"
  | TVarArgs -> "..."

and gen_params nest l =
  String.concat ", " (List.map (fun (name, ty) -> gen_decl nest name ty) l)

and gen_fields nest l =
  String.concat ";\n"
    (List.map
       (fun (name, ty) -> gen_decl (nest + 1) name ty |> gen_ident (nest + 1))
       l)
  ^ ";\n"

and gen_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | LShift -> "<<"
  | RShift -> ">>"
  | BitAnd -> "&"
  | BitXor -> "^"
  | BitOr -> "|"
  | LogAnd -> "&&"
  | LogOr -> "||"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | Comma -> ","

and gen_unop = function
  | Inc -> "++"
  | Dec -> "--"
  | Plus -> "+"
  | Minus -> "-"
  | BitNot -> "~"
  | LogNot -> "!"
  | Ref -> "&"
  | Deref -> "*"
  | Sizeof -> "sizeof "

and gen_value = function
  | VChar str -> "'" ^ str ^ "'"
  | VInt str | VFloat str -> str
  | VStr str -> "\"" ^ str ^ "\""
  | VNull -> "NULL"

and gen_buildin = function
  | VarStart (l, r) ->
      "__builtin_va_start (" ^ gen_expr l ^ ", " ^ gen_expr r ^ ")"
  | VarArg (l, r) -> "__builtin_va_arg (" ^ gen_expr l ^ ", " ^ gen_expr r ^ ")"
  | VarEnd e -> "__builtin_va_end (" ^ gen_expr e ^ ")"

and gen_expr = function
  | EBuildin buildin -> gen_buildin buildin
  | EConst v -> gen_value v
  | EVar (id, _) -> (
      match List.nth (List.rev !Env.program) id with
      | Param ((name, _), _, _)
      | Decl ((name, _), _, _)
      | GDecl (name, _)
      | LDecl (_, (name, _))
      | VarDef ((name, _), _, _, _)
      | GVarDef ((name, _), _)
      | FunctionDef ((name, _), _, _) ->
          name
      | LFunctionDef (_, (name, _), _, _) -> name
      | _ -> failwith "gen_expr")
  | EBinary (bin, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ gen_binop bin ^ " " ^ gen_expr rhs ^ ")"
  | EAssign (None, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ "=" ^ " " ^ gen_expr rhs ^ ")"
  | EAssign (Some bin, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ gen_binop bin ^ "=" ^ " " ^ gen_expr rhs ^ ")"
  | EUnary (un, expr) -> "(" ^ gen_unop un ^ gen_expr expr ^ ")"
  | ESizeof ty -> "sizeof " ^ gen_decl 0 "" ty
  | EPostfix (expr, postfix) -> gen_expr expr ^ gen_postfix postfix
  | ECond (cond, lhs, rhs) ->
      gen_expr cond ^ " ? " ^ gen_expr lhs ^ " : " ^ gen_expr rhs
  | ECast (ty, expr) -> "(" ^ gen_decl 0 "" ty ^ ")" ^ gen_expr expr
  | ECompoundLit (ty, init) -> "(" ^ gen_decl 0 "" ty ^ ")" ^ gen_init init

and gen_postfix = function
  | PCall l ->
      "(" ^ String.concat ", " (List.map (fun expr -> gen_arg expr) l) ^ ")"
  | PIdx expr -> "[" ^ gen_expr expr ^ "]"
  | PDot mem -> "." ^ mem
  | PArrow mem -> "->" ^ mem
  | PInc -> "++"
  | PDec -> "--"

and gen_arg = function
  | AExpr expr -> gen_expr expr
  | ADepth (Depth (name, _)) -> name
  | ADepth _ -> "NULL"

and gen_init = function
  | IScal expr -> gen_expr expr
  | IVect l ->
      "{"
      ^ String.concat ", "
          (List.map
             (fun (design, init) ->
               let str = gen_design design in
               (if str = "" then "" else str ^ " = ") ^ gen_init init)
             l)
      ^ "}"

and gen_design = function
  | Dnone -> ""
  | DIdx (expr, design) -> "[" ^ gen_expr expr ^ "]" ^ gen_design design
  | DField (mem, design) -> "." ^ mem ^ gen_design design

let rec gen_stmt nest = function
  | SDef l ->
      String.concat ""
        (List.map
           (fun id -> gen_item nest (List.nth (List.rev !Env.program) id))
           l)
  | SUnsafe [] | SStmts (_, []) -> ""
  | SUnsafe l ->
      ("{\n" |> gen_ident nest)
      ^ String.concat ""
          (List.map (fun stmt -> gen_stmt (nest + 1) stmt ^ "\n") l)
      ^ ("}" |> gen_ident nest)
  | SStmts (Depth ("", _), l) ->
      ("{\n" |> gen_ident nest)
      ^ String.concat ""
          (List.map (fun stmt -> gen_stmt (nest + 1) stmt ^ "\n") l)
      ^ ("}" |> gen_ident nest)
  | SStmts (Depth (name, _), l) ->
      ("{\n" |> gen_ident nest)
      ^ ("mi_heap_t* " ^ name ^ " = mi_heap_new();\n" |> gen_ident (nest + 1))
      ^ String.concat ""
          (List.map (fun stmt -> gen_stmt (nest + 1) stmt ^ "\n") l)
      ^ ("mi_heap_destroy(" ^ name ^ ");\n" |> gen_ident (nest + 1))
      ^ ("}" |> gen_ident nest)
  | SStmts (_, l) ->
      ("{\n" |> gen_ident nest)
      ^ String.concat ""
          (List.map (fun stmt -> gen_stmt (nest + 1) stmt ^ "\n") l)
      ^ ("}" |> gen_ident nest)
  | SWhile (expr, stmt) ->
      ("while (" ^ gen_expr expr ^ ")\n" |> gen_ident nest)
      ^ gen_stmt nest stmt ^ "\n"
  | SDoWhile (stmt, expr) ->
      ("do" ^ gen_stmt nest stmt ^ "\n" |> gen_ident nest)
      ^ ("while (" ^ gen_expr expr ^ ")" |> gen_ident nest)
  | SFor (stmt1, expr1, expr2, stmt2) ->
      ("for (" ^ gen_stmt 0 stmt1 ^ " "
       ^ (match expr1 with Some expr -> gen_expr expr | None -> "")
       ^ "; "
       ^ (match expr2 with Some expr -> gen_expr expr | None -> "")
       ^ ")\n"
      |> gen_ident nest)
      ^ gen_stmt nest stmt2 ^ "\n"
  | SIfElse (expr, stmt1, stmt2) ->
      ("if (" ^ gen_expr expr ^ ")\n" |> gen_ident nest)
      ^ gen_stmt nest stmt1 ^ "\n"
      ^
      let str = gen_stmt nest stmt2 ^ "\n" in
      if str = "\n" then "" else ("else\n" |> gen_ident nest) ^ str
  | SReturn None -> "return;"
  | SReturn (Some expr) -> "return " ^ gen_expr expr ^ ";" |> gen_ident nest
  | SLabel (label, stmt) -> label ^ ":\n" ^ gen_stmt nest stmt |> gen_ident nest
  | SGoto label -> "goto " ^ label ^ ";" |> gen_ident nest
  | SContinue -> "continue;" |> gen_ident nest
  | SBreak -> "break;" |> gen_ident nest
  | SSwitch (expr, stmt) ->
      ("switch (" ^ gen_expr expr ^ ")\n" |> gen_ident nest)
      ^ gen_stmt nest stmt ^ "\n"
  | SCase (expr, l) ->
      ("case " ^ gen_expr expr ^ ":\n" |> gen_ident nest)
      ^ String.concat "" (List.map (fun stmt -> gen_stmt nest stmt ^ "\n") l)
  | SDefault l ->
      ("default:\n" |> gen_ident nest)
      ^ String.concat "" (List.map (fun stmt -> gen_stmt nest stmt ^ "\n") l)
  | SExpr None -> ";" |> gen_ident nest
  | SExpr (Some expr) -> gen_expr expr ^ ";" |> gen_ident nest

and gen_item nest = function
  | Decl ((name, ty), _, _) -> gen_decl nest name ty ^ ";" |> gen_ident nest
  | VarDef ((name, ty), init, _, _) ->
      gen_decl nest name ty ^ " = " ^ gen_init init ^ ";" |> gen_ident nest
  | _ -> ""

and gen_item_global = function
  | GDecl (name, ty) | LDecl (_, (name, ty)) -> gen_decl 0 name ty ^ ";" ^ "\n"
  | GVarDef ((name, ty), init) ->
      gen_decl 0 name ty ^ " = " ^ gen_init init ^ ";" ^ "\n"
  | FunctionDef ((name, ty), _, stmt) ->
      "\n" ^ gen_decl 0 name ty ^ " " ^ gen_stmt 0 stmt ^ "\n"
  | LFunctionDef (_, (name, ty), _, stmt) ->
      "\n" ^ gen_decl 0 name ty ^ " " ^ gen_stmt 0 stmt ^ "\n"
  | _ -> ""

let prelude =
  "#include <mimalloc.h>\n\n#include <stdbool.h>\n\n#include <stddef.h>\n\n"

let gen_program program =
  prelude ^ String.concat "" (List.map gen_item_global program)
