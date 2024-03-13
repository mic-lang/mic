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

and ty = expr Syntax.ty [@@deriving show]
and typed_program = expr Syntax.item list [@@deriving show]
and typed_programi = (Syntax.id * expr Syntax.item) list [@@deriving show]

let program : typed_program ref = ref []

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
        {
          pointee_ty =
            TFun (type_conv ty, List.map (fun (n, ty) -> (n, type_conv ty)) decl);
          pointee_depth = Global;
          pointee_kind = Static;
          pointee_qual = [ Const ];
        }
  | TArr (ty, _) ->
      TPtr
        {
          pointee_ty = type_conv ty;
          pointee_depth = Decayed;
          pointee_kind = Unknown;
          pointee_qual = [ Const ];
        }
  | TPtr
      {
        pointee_ty = TFun (ty, decl);
        pointee_depth = depth;
        pointee_kind = kind;
        pointee_qual = qual;
      } ->
      TPtr
        {
          pointee_ty =
            TFun (type_conv ty, List.map (fun (n, ty) -> (n, type_conv ty)) decl);
          pointee_depth = depth;
          pointee_kind = kind;
          pointee_qual = qual;
        }
  | TPtr
      {
        pointee_ty = ty;
        pointee_depth = depth;
        pointee_kind = kind;
        pointee_qual = qual;
      } ->
      TPtr
        {
          pointee_ty = type_conv ty;
          pointee_depth = depth;
          pointee_kind = kind;
          pointee_qual = qual;
        }
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
                | TsVoid | TsStruct _ | TsUnion _ | TsStructDef _ | TsUnionDef _
                | TsTypedef _ ->
                    true
                | _ -> false)
              l
          in
          TDeclSpec [ e ]
        with _ -> failwith "type_conv")

let rec type_expr = function
  | Syntax.EConst v -> (
      match v with
      | VStr _ ->
          EConst
            ( Syntax.TPtr
                {
                  pointee_ty = TDeclSpec [ TsChar ];
                  pointee_depth = Global;
                  pointee_kind = Static;
                  pointee_qual = [ Const ];
                },
              v )
      | _ -> EConst (Syntax.TDeclSpec [ TsInt ], v))
  | Syntax.EVar id -> (
      match List.nth (List.rev !Env.program) id with
      | Decl decl | VarDef (decl, _) | FunctionDef (decl, _) ->
          EVar (type_conv (snd decl), id)
      | item -> failwith ("type_expr" ^ Syntax.show_item_ item))
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
      EUnary
        ( TPtr
            {
              pointee_ty = get_expr_ty expr;
              pointee_depth = Global;
              pointee_kind = Static;
              pointee_qual = [ Const ];
            },
          Ref,
          expr )
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
      | _ ->
          print_endline (show_ty (type_conv (get_expr_ty expr)));
          failwith "type_expr")
  | Syntax.EPostfix (expr, PIdx idx) ->
      let expr = type_expr expr in
      EPostfix
        (Syntax.get_base_ty (get_expr_ty expr), expr, PIdx (type_expr idx))
  | Syntax.EPostfix (expr, PDot name) -> (
      let expr = type_expr expr in
      match get_expr_ty expr with
      | TDeclSpec [ (TsStruct (id, _) | TsUnion (id, _)) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, _, mems) | UnionDef (_, _, mems) ->
              EPostfix (type_conv (List.assoc name mems), expr, PDot name)
          | _ -> failwith "type_expr: dot")
      | _ -> failwith "type_expr: dot")
  | Syntax.EPostfix (expr, PArrow name) -> (
      let expr = type_expr expr in
      match Syntax.get_base_ty (get_expr_ty expr) with
      | TDeclSpec [ (TsStruct (id, _) | TsUnion (id, _)) ] -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, _, mems) | UnionDef (_, _, mems) ->
              EPostfix (type_conv (List.assoc name mems), expr, PArrow name)
          | _ -> failwith "type_expr: arrow")
      | _ -> failwith "type_expr: arrow")
  | Syntax.EPostfix (expr, (PInc | PDec)) -> type_expr expr
  | Syntax.ECond (_, lhs, _) -> type_expr lhs
  | Syntax.ECast (ty, expr) -> ECast (type_conv ty, type_expr expr)
  | Syntax.ECompoundLit (ty, init) ->
      ECompoundLit (type_conv ty, type_init (type_conv ty) init)

and type_init ty init =
  match init with
  | Syntax.IScal expr -> Syntax.IScal (type_expr expr)
  | Syntax.IVect l -> (
      let loc = ref 0 in
      match ty with
      | Syntax.TArr (ty, _) ->
          let rec aux = function
            | [] -> []
            | (design, init) :: xs ->
                let ty, design = type_design ty design loc in
                (design, type_init ty init) :: aux xs
          in
          IVect (aux l)
      | Syntax.TDeclSpec [ TsStruct (id, _) ] ->
          let mems =
            match List.nth (List.rev !Env.program) id with
            | StructDef (_, _, mems) -> mems
            | _ -> failwith "type_init"
          in
          let rec aux loc mems l =
            match (mems, l) with
            | _, [] -> []
            | [], _ -> failwith "type_init: excess elements"
            | (_, memty) :: _, (design, init) :: xs ->
                let memty, design =
                  match design with
                  | Syntax.Dnone ->
                      loc := !loc + 1;
                      (type_conv memty, Syntax.Dnone)
                  | _ -> type_design (type_conv ty) design loc
                in
                let rec remain_mems mems x =
                  match (mems, x) with
                  | mems, 0 -> mems
                  | [], _ -> failwith "type_init error: excess elements"
                  | _ :: mems, x -> remain_mems mems (x - 1)
                in
                (design, type_init memty init)
                :: aux loc (remain_mems mems !loc) xs
          in
          IVect (aux loc mems l)
      | _ -> failwith "type_init: invalid type or initializer")

and type_design ty design loc =
  match (ty, design) with
  | Syntax.TArr (ty, _), DIdx (expr, design) ->
      let ty, design = type_design ty design loc in
      (ty, DIdx (type_expr expr, design))
  | Syntax.TDeclSpec [ TsStruct (id, _) ], DField (name, design) ->
      let mems =
        match List.nth (List.rev !Env.program) id with
        | StructDef (_, _, mems) -> mems
        | _ -> failwith "type_init"
      in
      let ty = try List.assoc name mems with _ -> failwith "type_design" in
      let l = List.init (List.length mems) (fun x -> x) in
      let l = List.map2 (fun (name, _) loc -> (name, loc)) mems l in
      loc := List.assoc name l;
      let ty, design = type_design ty design (ref 0) in
      (ty, DField (name, design))
  | ty, Dnone -> (type_conv ty, Dnone)
  | _ -> failwith ("type_design" ^ Syntax.show_ty_ ty ^ Syntax.show_desig design)

let rec type_stmt =
  let open Syntax in
  function
  | SDef l -> SDef l
  | SStmts (d, stmts) -> SStmts (d, List.map type_stmt stmts)
  | SWhile (expr, stmt) -> SWhile (type_expr expr, type_stmt stmt)
  | SDoWhile (stmt, expr) -> SDoWhile (type_stmt stmt, type_expr expr)
  | SFor (stmt1, expr1, expr2, stmt2) ->
      SFor
        ( type_stmt stmt1,
          Option.map type_expr expr1,
          Option.map type_expr expr2,
          type_stmt stmt2 )
  | SIfElse (expr, stmt1, stmt2) ->
      SIfElse (type_expr expr, type_stmt stmt1, type_stmt stmt2)
  | SReturn expr -> SReturn (Option.map type_expr expr)
  | SLabel (name, stmt) -> SLabel (name, type_stmt stmt)
  | SGoto name -> SGoto name
  | SContinue -> SContinue
  | SBreak -> SBreak
  | SSwitch (expr, stmt) -> SSwitch (type_expr expr, type_stmt stmt)
  | SCase (expr, stmts) -> SCase (type_expr expr, List.map type_stmt stmts)
  | SDefault stmts -> SDefault (List.map type_stmt stmts)
  | SExpr expr -> SExpr (Option.map type_expr expr)

let rec type_program =
  let open Syntax in
  function
  | [] -> []
  | Block (d, n) :: xs -> Block (d, n) :: type_program xs
  | Kind n :: xs -> Kind n :: type_program xs
  | Decl (n, ty) :: xs -> Decl (n, type_conv ty) :: type_program xs
  | GDecl (n, ty) :: xs -> GDecl (n, type_conv ty) :: type_program xs
  | StructDecl n :: xs -> StructDecl n :: type_program xs
  | UnionDecl n :: xs -> UnionDecl n :: type_program xs
  | EnumDecl n :: xs -> EnumDecl n :: type_program xs
  | VarDef ((n, ty), init) :: xs ->
      VarDef ((n, type_conv ty), type_init (type_conv ty) init)
      :: type_program xs
  | GVarDef ((n, ty), init) :: xs ->
      GVarDef ((n, type_conv ty), type_init (type_conv ty) init)
      :: type_program xs
  | StructDef (n, lp, l) :: xs ->
      StructDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | UnionDef (n, lp, l) :: xs ->
      UnionDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | EnumDef (n, l) :: xs -> EnumDef (n, l) :: type_program xs
  | FunctionDef ((n, ty), stmt) :: xs ->
      FunctionDef ((n, type_conv ty), type_stmt stmt) :: type_program xs
  | LFunctionDef (lparams, (n, ty), stmt) :: xs ->
      LFunctionDef (lparams, (n, type_conv ty), type_stmt stmt)
      :: type_program xs
