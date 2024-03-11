open Syntax

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
      | StructDef (name, _) -> name
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
      | UnionDef (name, _) -> name
      | _ -> failwith "gen_declspec")
  | TsUnionDef id -> (
      "union "
      ^
      match List.nth (List.rev !Env.program) id with
      | UnionDef (name, l) -> name ^ " {\n" ^ gen_fields l ^ "}"
      | _ -> failwith "gen_declspec")
  | TsTypedef id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl (name, _) | GDecl (name, _) -> name
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
  | TArr (ty, expr) -> gen_decl (str ^ "[" ^ gen_expr expr ^ "]") ty
  | TFun (ty, l) -> gen_decl (str ^ "(" ^ gen_params l ^ ")") ty
  | TDeclSpec l -> gen_declspecs l ^ if str = "" then "" else " " ^ str

and gen_params l =
  String.concat ", " (List.map (fun (name, ty) -> gen_decl name ty) l)

and gen_fields l =
  String.concat ";\n" (List.map (fun (name, ty) -> gen_decl name ty) l) ^ ";\n"

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

and gen_expr = function
  | EConst v -> gen_value v
  | EVar id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl (name, _)
      | GDecl (name, _)
      | LDecl (_, _, (name, _))
      | VarDef ((name, _), _)
      | GVarDef ((name, _), _)
      | FunctionDef ((name, _), _) ->
          name
      | LFunctionDef (_, _, (name, _), _) -> name
      | _ -> failwith "gen_expr")
  | EBinary (bin, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ gen_binop bin ^ " " ^ gen_expr rhs ^ ")"
  | EAssign (None, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ "=" ^ " " ^ gen_expr rhs ^ ")"
  | EAssign (Some bin, lhs, rhs) ->
      "(" ^ gen_expr lhs ^ " " ^ gen_binop bin ^ "=" ^ " " ^ gen_expr rhs ^ ")"
  | EUnary (un, expr) -> gen_unop un ^ gen_expr expr
  | ESizeof ty -> "sizeof " ^ gen_decl "" ty
  | EPostfix (expr, postfix) -> gen_expr expr ^ gen_postfix postfix
  | ECond (cond, lhs, rhs) ->
      gen_expr cond ^ " ? " ^ gen_expr lhs ^ " : " ^ gen_expr rhs
  | ECast (ty, expr) -> "(" ^ gen_decl "" ty ^ ")" ^ gen_expr expr
  | ECompoundLit (ty, init) -> "(" ^ gen_decl "" ty ^ ")" ^ gen_init init

and gen_postfix = function
  | PCall l ->
      "(" ^ String.concat ", " (List.map (fun expr -> gen_expr expr) l) ^ ")"
  | PIdx expr -> "[" ^ gen_expr expr ^ "]"
  | PDot mem -> "." ^ mem
  | PArrow mem -> "->" ^ mem
  | PInc -> "++"
  | PDec -> "--"

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

let rec gen_stmt = function
  | SDef l ->
      String.concat ""
        (List.map (fun id -> gen_item (List.nth (List.rev !Env.program) id)) l)
  | SStmts (_, []) -> ""
  | SStmts (_, l) ->
      "{\n"
      ^ String.concat "" (List.map (fun stmt -> gen_stmt stmt ^ "\n") l)
      ^ "}"
  | SWhile (expr, stmt) ->
      "while (" ^ gen_expr expr ^ ")\n" ^ gen_stmt stmt ^ "\n"
  | SDoWhile (stmt, expr) ->
      "do" ^ gen_stmt stmt ^ "\n" ^ "while (" ^ gen_expr expr ^ ")"
  | SFor (stmt1, expr1, expr2, stmt2) ->
      "for (" ^ gen_stmt stmt1 ^ " "
      ^ (match expr1 with Some expr -> gen_expr expr | None -> "")
      ^ "; "
      ^ (match expr2 with Some expr -> gen_expr expr | None -> "")
      ^ ")\n" ^ gen_stmt stmt2 ^ "\n"
  | SIfElse (expr, stmt1, stmt2) ->
      "if (" ^ gen_expr expr ^ ")\n" ^ gen_stmt stmt1 ^ "\n"
      ^
      let str = gen_stmt stmt2 ^ "\n" in
      if str = "\n" then "" else "else\n" ^ str
  | SReturn None -> "return;"
  | SReturn (Some expr) -> "return " ^ gen_expr expr ^ ";"
  | SLabel (label, stmt) -> label ^ ":\n" ^ gen_stmt stmt
  | SGoto label -> "goto " ^ label ^ ";"
  | SContinue -> "continue;"
  | SBreak -> "break;"
  | SSwitch (expr, stmt) ->
      "switch (" ^ gen_expr expr ^ ")\n" ^ gen_stmt stmt ^ "\n"
  | SCase (expr, l) ->
      "case " ^ gen_expr expr ^ ":\n"
      ^ String.concat "" (List.map (fun stmt -> gen_stmt stmt ^ "\n") l)
  | SDefault l ->
      "default:\n"
      ^ String.concat "" (List.map (fun stmt -> gen_stmt stmt ^ "\n") l)
  | SExpr None -> ";"
  | SExpr (Some expr) -> gen_expr expr ^ ";"

and gen_item = function
  | Decl (name, ty) -> gen_decl name ty ^ ";"
  | VarDef ((name, ty), init) -> gen_decl name ty ^ " = " ^ gen_init init ^ ";"
  | _ -> ""

and gen_item_global = function
  | GDecl (name, ty) -> gen_decl name ty ^ ";" ^ "\n"
  | LDecl (_, _, (name, ty)) -> gen_decl name ty ^ ";" ^ "\n"
  | GVarDef ((name, ty), init) ->
      gen_decl name ty ^ " = " ^ gen_init init ^ ";" ^ "\n"
  | FunctionDef ((name, ty), stmt) ->
      "\n" ^ gen_decl name ty ^ " " ^ gen_stmt stmt ^ "\n"
  | LFunctionDef (_, _, (name, ty), stmt) ->
      "\n" ^ gen_decl name ty ^ " " ^ gen_stmt stmt ^ "\n"
  | _ -> ""

let gen_program program = String.concat "" (List.map gen_item_global program)
