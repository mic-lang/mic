type expr =
  | EConst of expr Syntax.ty * Syntax.value
  | EVar of expr Syntax.ty * Syntax.id
  | EBinary of expr Syntax.ty * Syntax.binary * expr * expr
  | EAssign of expr Syntax.ty * Syntax.binary option * expr * expr
  | EUnary of expr Syntax.ty * Syntax.unary * expr
  | ESizeof of expr Syntax.ty * expr Syntax.ty
  | EPostfix of expr Syntax.ty * expr * expr Syntax.postfix
  | ECond of expr Syntax.ty * expr * expr * expr
  | ECast of expr Syntax.ty * expr
  | ECompoundLit of expr Syntax.ty * expr Syntax.init
[@@deriving show]

let get_expr_ty = function
  | EConst (ty, _)
  | EVar (ty, _)
  | EBinary (ty, _, _, _)
  | EAssign (ty, _, _, _)
  | EUnary (ty, _, _)
  | ESizeof (ty, _)
  | EPostfix (ty, _, _)
  | ECond (ty, _, _, _)
  | ECast (ty, _)
  | ECompoundLit (ty, _) ->
      ty

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

let rec type_expr = function
  | Syntax.EConst v -> (
      match v with
      | VStr _ -> EConst (Syntax.TPtr (TDeclSpec [ TsChar ]), v)
      | _ -> EConst (Syntax.TPtr (TDeclSpec [ TsInt ]), v))
  | Syntax.EVar id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl decl | VarDef (decl, _) -> EVar (type_conv (snd decl), id)
      | _ -> failwith "type_expr")
  | Syntax.EBinary
      ( (( Add | Sub | Mul | Div | Mod | LShift | RShift | BitAnd | BitXor
         | BitOr ) as bin),
        lhs,
        rhs ) ->
      let lhs = type_expr lhs in
      EBinary (get_expr_ty lhs, bin, lhs, type_expr rhs)
  | Syntax.EBinary
      (((LogAnd | LogOr | Lt | Le | Gt | Ge | Eq | Ne) as bin), lhs, rhs) ->
      EBinary (TDeclSpec [ TsInt ], bin, type_expr lhs, type_expr rhs)
  | Syntax.EBinary (Comma, lhs, rhs) ->
      let rhs = type_expr rhs in
      EBinary (get_expr_ty rhs, Comma, type_expr lhs, rhs)
  | Syntax.EAssign (bin, lhs, rhs) ->
      let lhs = type_expr lhs in
      EAssign (get_expr_ty lhs, bin, lhs, type_expr rhs)
  | Syntax.EUnary (((Plus | Minus | BitNot | LogNot) as un), expr) ->
      let expr = type_expr expr in
      EUnary (get_expr_ty expr, un, expr)
  | Syntax.EUnary (Ref, expr) ->
      let expr = type_expr expr in
      EUnary (TPtr (get_expr_ty expr), Ref, expr)
  | Syntax.EUnary (Deref, expr) ->
      let expr = type_expr expr in
      EUnary (type_conv (Syntax.get_base_ty (get_expr_ty expr)), Deref, expr)
  | Syntax.EUnary (Sizeof, expr) ->
      EUnary (TDeclSpec [ TsInt ], Sizeof, type_expr expr)
  | Syntax.ESizeof ty -> ESizeof (TDeclSpec [ TsInt ], type_conv ty)
  | Syntax.EPostfix (expr, PCall params) -> (
      let expr = type_expr expr in
      let params = List.map type_expr params in
      match Syntax.get_base_ty (type_conv (get_expr_ty expr)) with
      | TFun (ret, _) -> EPostfix (ret, expr, PCall params)
      | _ -> failwith "type_expr")
  | Syntax.EPostfix (expr, PIdx idx) ->
      let expr = type_expr expr in
      EPostfix
        (Syntax.get_base_ty (get_expr_ty expr), expr, PIdx (type_expr idx))
  | Syntax.EPostfix (expr, PDot name) -> (
    let expr = type_expr expr in
      match get_expr_ty expr with
      | TDeclSpec [ (TsStruct id | TsUnion id) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, mems) | UnionDef (_, mems) ->
              EPostfix(type_conv (List.assoc name mems), expr, PDot name)
          | _ -> failwith "type_expr")
      | _ -> failwith "type_expr")
  | Syntax.EPostfix (expr, PArrow name) -> (
    let expr = type_expr expr in
      match Syntax.get_base_ty (get_expr_ty expr) with
      | TDeclSpec [ (TsStruct id | TsUnion id) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, mems) | UnionDef (_, mems) ->
              EPostfix(type_conv (List.assoc name mems), expr, PArrow name)
          | _ -> failwith "type_expr")
      | _ -> failwith "type_expr")
  | Syntax.EPostfix (expr, (PInc | PDec)) -> type_expr expr
  | Syntax.ECond (_, lhs, _) -> type_expr lhs
  | Syntax.ECast (ty, expr) -> ECast(type_conv ty, type_expr expr)
  | Syntax.ECompoundLit (ty, _) -> ECompoundLit(type_conv ty, IVect [])
