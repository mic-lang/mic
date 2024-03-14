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
  | TVar
      {
        ownership;
        var_ty = ty;
        var_depth = depth;
        var_kind = kind;
        var_qual = qual;
      } ->
      TVar
        {
          ownership;
          var_ty = type_conv ty;
          var_depth = depth;
          var_kind = kind;
          var_qual = qual;
        }
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

let used_var_type env = function
  | Syntax.TVar { ownership = { contents = Moved depth } as ownership; _ } ->
      if List.mem depth env then ownership := Has
      else failwith "used var dropped before"
  | _ -> ()

let rec type_expr env = function
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
      | Decl (decl, depth, ownership) | VarDef (decl, _, depth, ownership) ->
          print_endline (fst decl);
          EVar
            ( TVar
                {
                  ownership;
                  var_ty = type_conv (snd decl);
                  var_depth = depth;
                  var_kind = Auto;
                  var_qual = [];
                },
              id )
      | GDecl decl
      | GVarDef (decl, _)
      | FunctionDef (decl, _)
      | LFunctionDef (_, decl, _) ->
          print_endline (fst decl);
          EVar
            ( TVar
                {
                  ownership = ref Syntax.Has;
                  var_ty = type_conv (snd decl);
                  var_depth = Global;
                  var_kind = Static;
                  var_qual = [];
                },
              id )
      | item -> failwith ("type_expr env: var " ^ Syntax.show_item_ item))
  | Syntax.EBinary
      ( (( Add | Sub | Mul | Div | Mod | LShift | RShift | BitAnd | BitXor
         | BitOr ) as bin),
        lhs,
        rhs ) ->
      let lhs = type_expr env lhs in
      EBinary
        (Syntax.get_contents_ty (get_expr_ty lhs), bin, lhs, type_expr env rhs)
  | Syntax.EBinary
      (((LogAnd | LogOr | Lt | Le | Gt | Ge | Eq | Ne) as bin), lhs, rhs) ->
      EBinary (TDeclSpec [ TsInt ], bin, type_expr env lhs, type_expr env rhs)
  | Syntax.EBinary (Comma, lhs, rhs) ->
      let rhs = type_expr env rhs in
      EBinary
        (Syntax.get_contents_ty (get_expr_ty rhs), Comma, type_expr env lhs, rhs)
  | Syntax.EAssign (bin, lhs, rhs) ->
      let lhs = type_expr env lhs in
      let rhs = type_expr env rhs in
      (match (get_expr_ty lhs, get_expr_ty rhs) with
      | TVar { var_depth = depth; _ }, TVar { ownership; _ } ->
          ownership := Moved depth
      | _ -> ());
      EAssign (get_expr_ty lhs, bin, lhs, rhs)
  | Syntax.EUnary (((Inc | Dec | Plus | Minus | BitNot | LogNot) as un), expr)
    ->
      let expr = type_expr env expr in
      EUnary (get_expr_ty expr, un, expr)
  | Syntax.EUnary (Ref, expr) -> (
      let expr = type_expr env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty = ty;
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } ->
          EUnary
            ( TVar
                {
                  ownership;
                  var_ty =
                    TPtr
                      {
                        pointee_ty = ty;
                        pointee_depth = depth;
                        pointee_kind = kind;
                        pointee_qual = qual;
                      };
                  var_depth = depth;
                  var_kind = kind;
                  var_qual = qual;
                },
              Ref,
              expr )
      | _ -> failwith "not lvalue")
  | Syntax.EUnary (Deref, expr) -> (
      let expr = type_expr env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty = ty;
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } ->
          EUnary
            ( TVar
                {
                  ownership;
                  var_ty = type_conv (Syntax.get_base_ty ty);
                  var_depth = depth;
                  var_kind = kind;
                  var_qual = qual;
                },
              Deref,
              expr )
      | ty -> failwith ("type_expr env : deref " ^ show_ty ty))
  | Syntax.EUnary (Sizeof, expr) ->
      EUnary (TDeclSpec [ TsInt ], Sizeof, type_expr env expr)
  | Syntax.ESizeof ty -> ESizeof (TDeclSpec [ TsInt ], type_conv ty)
  | Syntax.EPostfix (expr, PCall params) -> (
      let expr = type_expr env expr in
      let params = List.map (type_expr env) params in
      match
        Syntax.get_base_ty
          (Syntax.get_contents_ty (type_conv (get_expr_ty expr)))
      with
      | TFun (ret, _) -> EPostfix (ret, expr, PCall params)
      | _ ->
          print_endline (show_ty (type_conv (get_expr_ty expr)));
          failwith "type_expr env")
  | Syntax.EPostfix (expr, PIdx idx) -> (
      let expr = type_expr env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty = ty;
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } ->
          EPostfix
            ( TVar
                {
                  ownership;
                  var_ty = type_conv (Syntax.get_base_ty ty);
                  var_depth = depth;
                  var_kind = kind;
                  var_qual = qual;
                },
              expr,
              PIdx (type_expr env idx) )
      | ty -> failwith ("type_expr env : idx " ^ show_ty ty))
  | Syntax.EPostfix (expr, PDot name) -> (
      let expr = type_expr env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty =
              TDeclSpec
                [
                  ( TsStruct (id, _)
                  | TsUnion (id, _)
                  | TsStructDef id
                  | TsUnionDef id );
                ];
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, _, mems) | UnionDef (_, _, mems) ->
              EPostfix
                ( TVar
                    {
                      ownership;
                      var_ty = type_conv (List.assoc name mems);
                      var_depth = depth;
                      var_kind = kind;
                      var_qual = qual;
                    },
                  expr,
                  PDot name )
          | _ -> failwith "type_expr env: dot")
      | _ -> failwith "type_expr env: dot")
  | Syntax.EPostfix (expr, PArrow name) -> (
      let expr = type_expr env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty = ty;
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } -> (
          match Syntax.get_base_ty ty with
          | TDeclSpec
              [
                ( TsStruct (id, _)
                | TsUnion (id, _)
                | TsStructDef id
                | TsUnionDef id );
              ] -> (
              match List.nth (List.rev !Env.program) id with
              | StructDef (_, _, mems) | UnionDef (_, _, mems) ->
                  EPostfix
                    ( TVar
                        {
                          ownership;
                          var_ty = type_conv (List.assoc name mems);
                          var_depth = depth;
                          var_kind = kind;
                          var_qual = qual;
                        },
                      expr,
                      PArrow name )
              | _ -> failwith "type_expr env: arrow")
          | _ -> failwith "type_expr env: not a compound type")
      | _ ->
          print_endline (show_ty (Syntax.get_base_ty (get_expr_ty expr)));
          failwith "type_expr env: arrow")
  | Syntax.EPostfix (expr, ((PInc | PDec) as postfix)) ->
      EPostfix
        ( Syntax.get_contents_ty (get_expr_ty (type_expr env expr)),
          type_expr env expr,
          postfix )
  | Syntax.ECond (cond, lhs, rhs) ->
      ECond
        ( Syntax.get_contents_ty (get_expr_ty (type_expr env lhs)),
          type_expr env cond,
          type_expr env lhs,
          type_expr env rhs )
  | Syntax.ECast (ty, expr) -> ECast (type_conv ty, type_expr env expr)
  | Syntax.ECompoundLit (ty, init) ->
      ECompoundLit (type_conv ty, type_init env (type_conv ty) init)

and type_init env ty init =
  match init with
  | Syntax.IScal expr -> Syntax.IScal (type_expr env expr)
  | Syntax.IVect l -> (
      let loc = ref 0 in
      match ty with
      | Syntax.TArr (ty, _) ->
          let rec aux = function
            | [] -> []
            | (design, init) :: xs ->
                let ty, design = type_design env ty design loc in
                (design, type_init env ty init) :: aux xs
          in
          IVect (aux l)
      | Syntax.TDeclSpec [ TsStruct (id, _) ] ->
          let mems =
            match List.nth (List.rev !Env.program) id with
            | StructDef (_, _, mems) -> mems
            | _ -> failwith "type_init env"
          in
          let rec aux loc mems l =
            match (mems, l) with
            | _, [] -> []
            | [], _ -> failwith "type_init env: excess elements"
            | (_, memty) :: _, (design, init) :: xs ->
                let memty, design =
                  match design with
                  | Syntax.Dnone ->
                      loc := !loc + 1;
                      (type_conv memty, Syntax.Dnone)
                  | _ -> type_design env (type_conv ty) design loc
                in
                let rec remain_mems mems x =
                  match (mems, x) with
                  | mems, 0 -> mems
                  | [], _ -> failwith "type_init env error: excess elements"
                  | _ :: mems, x -> remain_mems mems (x - 1)
                in
                (design, type_init env memty init)
                :: aux loc (remain_mems mems !loc) xs
          in
          IVect (aux loc mems l)
      | _ -> failwith "type_init env: invalid type or initializer")

and type_design env ty design loc =
  match (ty, design) with
  | Syntax.TArr (ty, _), DIdx (expr, design) ->
      let ty, design = type_design env ty design loc in
      (ty, DIdx (type_expr env expr, design))
  | Syntax.TDeclSpec [ TsStruct (id, _) ], DField (name, design) ->
      let mems =
        match List.nth (List.rev !Env.program) id with
        | StructDef (_, _, mems) -> mems
        | _ -> failwith "type_init env"
      in
      let ty =
        try List.assoc name mems with _ -> failwith "type_design env"
      in
      let l = List.init (List.length mems) (fun x -> x) in
      let l = List.map2 (fun (name, _) loc -> (name, loc)) mems l in
      loc := List.assoc name l;
      let ty, design = type_design env ty design (ref 0) in
      (ty, DField (name, design))
  | ty, Dnone -> (type_conv ty, Dnone)
  | _ ->
      failwith
        ("type_design env" ^ Syntax.show_ty_ ty ^ Syntax.show_desig design)

let rec type_stmt env =
  let open Syntax in
  function
  | SDef l -> SDef l
  | SStmts (d, stmts) -> SStmts (d, List.map (type_stmt (d :: env)) stmts)
  | SWhile (expr, stmt) -> SWhile (type_expr env expr, type_stmt env stmt)
  | SDoWhile (stmt, expr) -> SDoWhile (type_stmt env stmt, type_expr env expr)
  | SFor (stmt1, expr1, expr2, stmt2) ->
      SFor
        ( type_stmt env stmt1,
          Option.map (type_expr env) expr1,
          Option.map (type_expr env) expr2,
          type_stmt env stmt2 )
  | SIfElse (expr, stmt1, stmt2) ->
      SIfElse (type_expr env expr, type_stmt env stmt1, type_stmt env stmt2)
  | SReturn expr -> SReturn (Option.map (type_expr env) expr)
  | SLabel (name, stmt) -> SLabel (name, type_stmt env stmt)
  | SGoto name -> SGoto name
  | SContinue -> SContinue
  | SBreak -> SBreak
  | SSwitch (expr, stmt) -> SSwitch (type_expr env expr, type_stmt env stmt)
  | SCase (expr, stmts) ->
      SCase (type_expr env expr, List.map (type_stmt env) stmts)
  | SDefault stmts -> SDefault (List.map (type_stmt env) stmts)
  | SExpr expr -> SExpr (Option.map (type_expr env) expr)

let rec type_program =
  let open Syntax in
  function
  | [] -> []
  | Block (d, n) :: xs -> Block (d, n) :: type_program xs
  | Kind n :: xs -> Kind n :: type_program xs
  | Decl ((n, ty), d, own) :: xs ->
      Decl ((n, type_conv ty), d, own) :: type_program xs
  | GDecl (n, ty) :: xs -> GDecl (n, type_conv ty) :: type_program xs
  | StructDecl n :: xs -> StructDecl n :: type_program xs
  | UnionDecl n :: xs -> UnionDecl n :: type_program xs
  | EnumDecl n :: xs -> EnumDecl n :: type_program xs
  | VarDef ((n, ty), init, d, own) :: xs ->
      VarDef ((n, type_conv ty), type_init [] (type_conv ty) init, d, own)
      :: type_program xs
  | GVarDef ((n, ty), init) :: xs ->
      GVarDef ((n, type_conv ty), type_init [] (type_conv ty) init)
      :: type_program xs
  | StructDef (n, lp, l) :: xs ->
      StructDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | UnionDef (n, lp, l) :: xs ->
      UnionDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | EnumDef (n, l) :: xs -> EnumDef (n, l) :: type_program xs
  | FunctionDef ((n, ty), stmt) :: xs ->
      FunctionDef ((n, type_conv ty), type_stmt [] stmt) :: type_program xs
  | LFunctionDef (lparams, (n, ty), stmt) :: xs ->
      LFunctionDef
        ( lparams,
          (n, type_conv ty),
          type_stmt (Syntax.filter_depth lparams) stmt )
      :: type_program xs
