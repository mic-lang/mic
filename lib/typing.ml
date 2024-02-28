type expr =
  | EConst of expr Syntax.ty * Syntax.value
  | EVar of expr Syntax.ty * Syntax.id
  | EBinary of expr Syntax.ty * Syntax.binary * expr * expr
  | EAssign of expr Syntax.ty * Syntax.binary * expr * expr
  | EUnary of expr Syntax.ty * Syntax.unary * expr
  | ESizeof of expr Syntax.ty * expr Syntax.ty
  | EPostfix of expr Syntax.ty * expr * expr Syntax.postfix
  | ECond of expr Syntax.ty * expr * expr * expr
  | ECast of expr Syntax.ty * expr
  | ECompoundLit of expr Syntax.ty * expr Syntax.init
[@@deriving show]

let rec type_conv =
  let open Syntax in
  function
  | TFun (ty, decl) ->
      TPtr
        (TFun (type_conv ty, List.map (fun (n, ty) -> (n, type_conv ty)) decl))
  | TArr (ty, _) -> TPtr (type_conv ty)
  | TConstPtr ty -> TConstPtr (type_conv ty)
  | TPtr ty -> TPtr (type_conv ty)
  | TDeclSpec l -> (
      if
        List.exists
          (function
            | TsInt | TsShort | TsLong | TsChar | TsSigned | TsUnsigned
            | TsFloat | TsDouble ->
                true
            | _ -> false)
          l
      then TDeclSpec [ TsInt ]
      else
        try
          let e =
            List.find
              (function
                | TsVoid | TsStruct _ | TsUnion _ | TsTypedef _ -> true
                | _ -> false)
              l
          in
          TDeclSpec [ e ]
        with _ -> failwith "type_conv")

let rec type_expr =
  let open Syntax in
  function
  | EConst v -> (
      match v with
      | VStr _ -> TPtr (TDeclSpec [ TsChar ])
      | _ -> TDeclSpec [ TsInt ])
  | EVar id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl decl -> type_conv (snd decl)
      | VarDef (decl, _) -> type_conv (snd decl)
      | _ -> failwith "type_expr")
  | EBinary
      ( (Add | Sub | Mul | Div | Mod | LShift | RShift | BitAnd | BitXor | BitOr),
        lhs,
        _ ) ->
      type_expr lhs
  | EBinary ((LogAnd | LogOr | Lt | Le | Gt | Ge | Eq | Ne), _, _) ->
      TDeclSpec [ TsInt ]
  | EBinary (Comma, lhs, _) -> type_expr lhs
  | EAssign (_, lhs, _) -> type_expr lhs
  | EUnary ((Plus | Minus | BitNot | LogNot), expr) -> type_expr expr
  | EUnary (Ref, expr) -> TPtr (type_expr expr)
  | EUnary (Deref, expr) ->
      type_conv (get_base_ty (type_expr expr))
  | EUnary (Sizeof, _) -> TDeclSpec [ TsInt ]
  | ESizeof _ -> TDeclSpec [ TsInt ]
  | EPostfix (expr, PCall _) -> (
      match get_base_ty (type_expr expr) with
      | TFun (ret, _) -> ret
      | _ -> failwith "type_expr")
  | EPostfix (expr, PIdx _) -> get_base_ty (type_expr expr)
  | EPostfix (expr, PDot name) -> (
      match type_expr expr with
      | TDeclSpec [ (TsStruct id | TsUnion id) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, mems) | UnionDef (_, mems) ->
              type_conv (List.assoc name mems)
          | _ -> failwith "type_expr")
      | _ -> failwith "type_expr")
  | EPostfix (expr, PArrow name) -> (
      match get_base_ty (type_expr expr) with
      | TDeclSpec [ (TsStruct id | TsUnion id) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, mems) | UnionDef (_, mems) ->
              type_conv (List.assoc name mems)
          | _ -> failwith "type_expr")
      | _ -> failwith "type_expr")
  | EPostfix (expr, (PInc | PDec)) -> type_expr expr
  | ECond (_, lhs, _) -> type_expr lhs
  | ECast (ty, _) | ECompoundLit (ty, _) -> type_conv ty
