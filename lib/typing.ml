type expr =
  | EConst of expr Syntax.ty * Syntax.value
  | EVar of expr Syntax.ty * Syntax.id * Syntax.lparam list
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
  | EVar (ty, _, _)
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
          pointee_ownership = ref Has;
          pointee_ty =
            TFun (type_conv ty, List.map (fun (n, ty) -> (n, type_conv ty)) decl);
          pointee_depth = Global;
          pointee_kind = Static;
          pointee_qual = [ Const ];
        }
  | TArr (ty, _) ->
      TPtr
        {
          pointee_ownership = ref Has;
          pointee_ty = type_conv ty;
          pointee_depth = Decayed;
          pointee_kind = Unknown;
          pointee_qual = [ Const ];
        }
  | TPtr
      {
        pointee_ownership = ownership;
        pointee_ty = TFun (ty, decl);
        pointee_depth = depth;
        pointee_kind = kind;
        pointee_qual = qual;
      } ->
      TPtr
        {
          pointee_ownership = ownership;
          pointee_ty =
            TFun (type_conv ty, List.map (fun (n, ty) -> (n, type_conv ty)) decl);
          pointee_depth = depth;
          pointee_kind = kind;
          pointee_qual = qual;
        }
  | TPtr
      {
        pointee_ownership = ownership;
        pointee_ty = ty;
        pointee_depth = depth;
        pointee_kind = kind;
        pointee_qual = qual;
      } ->
      TPtr
        {
          pointee_ownership = ownership;
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
  | TBlock -> TBlock

let used_var_type env = function
  | Syntax.TVar { ownership = { contents = Moved depth } as ownership; _ } ->
      if List.mem depth env then (
        print_endline (Syntax.show_depths env);
        failwith "used var dropped before")
      else ownership := Has
  | _ -> ()

let rec unify lparams largs =
  let open Syntax in
  match (lparams, largs) with
  | LBlock (Depth (name, _)) :: xs, LKind Static :: ys ->
      (name, LBlock Global) :: unify xs ys
  | LBlock (Depth (name, _)) :: xs, LBlock blk :: ys ->
      (name, LBlock blk) :: unify xs ys
  | LKind (User name) :: xs, LKind kind :: ys ->
      (name, LKind kind) :: unify xs ys
  | LBlock _ :: _, LKind _ :: _ | LKind _ :: _, LBlock _ :: _ ->
      failwith "the lifetime parameter mismatch"
  | _ :: xs, _ :: ys -> unify xs ys
  | _ :: _, [] | [], _ :: _ ->
      failwith "the length of lifetime parameter is incorrect"
  | [], [] -> []

let apply_depth subst =
  let open Syntax in
  function
  | Syntax.Depth (name, _) when List.mem_assoc name subst -> (
      match List.assoc name subst with
      | LBlock blk -> blk
      | _ -> failwith "apply_kind")
  | depth -> depth

let apply_kind subst =
  let open Syntax in
  function
  | User name when List.mem_assoc name subst -> (
      match List.assoc name subst with
      | LKind kind -> kind
      | _ -> failwith "apply_kind")
  | kind -> kind

let apply_lparams subst =
  let open Syntax in
  function
  | LBlock blk -> LBlock (apply_depth subst blk)
  | LKind kind -> LKind (apply_kind subst kind)

let rec apply subst =
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
          var_ty = apply subst ty;
          var_depth = apply_depth subst depth;
          var_kind = apply_kind subst kind;
          var_qual = qual;
        }
  | TFun (ty, params) ->
      TFun (apply subst ty, List.map (fun (n, ty) -> (n, apply subst ty)) params)
  | TPtr
      {
        pointee_ownership = ownership;
        pointee_ty = ty;
        pointee_depth = depth;
        pointee_kind = kind;
        pointee_qual = qual;
      } ->
      TPtr
        {
          pointee_ownership = ownership;
          pointee_ty = apply subst ty;
          pointee_depth = apply_depth subst depth;
          pointee_kind = apply_kind subst kind;
          pointee_qual = qual;
        }
  | TArr (ty, _) ->
      TPtr
        {
          pointee_ownership = ref Has;
          pointee_ty = apply subst ty;
          pointee_depth = Decayed;
          pointee_kind = Unknown;
          pointee_qual = [ Const ];
        }
  | TDeclSpec l ->
      let f = function
        | TsStruct (id, lparams) ->
            let largs = List.map (apply_lparams subst) lparams in
            (match List.nth (List.rev !Env.program) id with
            | StructDef (_, lparams, _) -> ignore (unify lparams largs)
            | _ -> failwith "apply");
            TsStruct (id, largs)
        | TsUnion (id, lparams) ->
            let largs = List.map (apply_lparams subst) lparams in
            (match List.nth (List.rev !Env.program) id with
            | UnionDef (_, lparams, _) -> ignore (unify lparams largs)
            | _ -> failwith "apply");
            TsUnion (id, largs)
        | ds -> ds
      in
      TDeclSpec (List.map f l)
  | TBlock -> TBlock

let rec check_ty =
  let open Syntax in
  function
  | TVar { var_ty = ty; _ } -> check_ty ty
  | TFun (ty, params) ->
      check_ty ty;
      List.iter (fun (_, ty) -> check_ty ty) params
  | TPtr { pointee_ty = ty; _ } -> check_ty ty
  | TArr (ty, _) -> check_ty ty
  | TDeclSpec l ->
      let f = function
        | TsStruct (id, largs) -> (
            match List.nth (List.rev !Env.program) id with
            | StructDef (_, lparams, _) -> ignore (unify lparams largs)
            | StructDecl _ -> ()
            | _ as item ->
                print_endline (Syntax.show_item_ item);
                failwith "struct error")
        | TsUnion (id, largs) -> (
            match List.nth (List.rev !Env.program) id with
            | UnionDef (_, lparams, _) -> ignore (unify lparams largs)
            | UnionDecl _ -> ()
            | _ -> failwith "union error")
        | _ -> ()
      in
      List.iter f l
  | TBlock -> ()

let rec type_expr is_unsafe env = function
  | Syntax.EConst v -> (
      match v with
      | VStr _ ->
          EConst
            ( Syntax.TPtr
                {
                  pointee_ownership = ref Syntax.Has;
                  pointee_ty = TDeclSpec [ TsChar ];
                  pointee_depth = Global;
                  pointee_kind = Static;
                  pointee_qual = [ Const ];
                },
              v )
      | _ -> EConst (Syntax.TDeclSpec [ TsInt ], v))
  | Syntax.EVar (id, largs) -> (
      match List.nth (List.rev !Env.program) id with
      | Param (decl, depth, ownership)
      | Decl (decl, depth, ownership)
      | VarDef (decl, _, depth, ownership) ->
          (*print_endline (fst decl);*)
          let ty =
            Syntax.TVar
              {
                ownership;
                var_ty = type_conv (snd decl);
                var_depth = depth;
                var_kind = Auto;
                var_qual = [];
              }
          in
          if not is_unsafe then used_var_type env ty;
          EVar (ty, id, [])
      | GDecl decl | GVarDef (decl, _) | FunctionDef (decl, _, _) ->
          (*print_endline (fst decl);*)
          let ty =
            Syntax.TVar
              {
                ownership = ref Syntax.Has;
                var_ty = type_conv (snd decl);
                var_depth = Global;
                var_kind = Static;
                var_qual = [];
              }
          in
          if not is_unsafe then used_var_type env ty;
          EVar (ty, id, [])
      | LFunctionDef (lparams, decl, _, _) | LDecl (lparams, decl) ->
          (*print_endline (fst decl);*)
          let ty =
            Syntax.TVar
              {
                ownership = ref Syntax.Has;
                var_ty = type_conv (snd decl);
                var_depth = Global;
                var_kind = Static;
                var_qual = [];
              }
          in
          let subst = unify lparams largs in
          let ty = apply subst ty in
          if not is_unsafe then used_var_type env ty;
          EVar (ty, id, largs)
      | item ->
          failwith ("type_expr is_unsafe env: var " ^ Syntax.show_item_ item))
  | Syntax.EBinary
      ( (( Add | Sub | Mul | Div | Mod | LShift | RShift | BitAnd | BitXor
         | BitOr ) as bin),
        lhs,
        rhs ) ->
      let lhs = type_expr is_unsafe env lhs in
      EBinary
        ( Syntax.get_contents_ty (get_expr_ty lhs),
          bin,
          lhs,
          type_expr is_unsafe env rhs )
  | Syntax.EBinary
      (((LogAnd | LogOr | Lt | Le | Gt | Ge | Eq | Ne) as bin), lhs, rhs) ->
      EBinary
        ( TDeclSpec [ TsInt ],
          bin,
          type_expr is_unsafe env lhs,
          type_expr is_unsafe env rhs )
  | Syntax.EBinary (Comma, lhs, rhs) ->
      let rhs = type_expr is_unsafe env rhs in
      EBinary
        ( Syntax.get_contents_ty (get_expr_ty rhs),
          Comma,
          type_expr is_unsafe env lhs,
          rhs )
  | Syntax.EAssign (bin, lhs, Syntax.ECompoundLit (ty, init)) ->
      let lhs = type_expr is_unsafe env lhs in
      let rhs =
        ECompoundLit
          ( type_conv ty,
            type_init is_unsafe env (type_conv (get_expr_ty lhs)) init )
      in
      (if not is_unsafe then
         match (get_expr_ty lhs, Syntax.get_contents_ty (get_expr_ty rhs)) with
         | ( TVar { var_depth = depth; _ },
             TPtr { pointee_ownership = ownership; _ } ) ->
             ownership := Moved depth
             (*print_endline ("ownership:" ^ show_ty (get_expr_ty rhs))*)
         | _ -> ());
      (match
         ( Syntax.get_contents_ty (get_expr_ty lhs),
           Syntax.get_contents_ty (get_expr_ty rhs) )
       with
      | TPtr lhs, TPtr rhs
        when lhs.pointee_depth <> rhs.pointee_depth
             || lhs.pointee_kind <> rhs.pointee_kind ->
          failwith "pointer type mismatch"
      | _ -> ());
      EAssign (get_expr_ty lhs, bin, lhs, rhs)
  | Syntax.EAssign (bin, lhs, rhs) ->
      let lhs = type_expr is_unsafe env lhs in
      let rhs = type_expr is_unsafe env rhs in
      (if not is_unsafe then
         match (get_expr_ty lhs, Syntax.get_contents_ty (get_expr_ty rhs)) with
         | ( TVar { var_depth = depth; _ },
             TPtr { pointee_ownership = ownership; _ } ) ->
             ownership := Moved depth
         (*print_endline ("ownership:" ^ show_ty (get_expr_ty rhs))*)
         | _ -> ());
      (match
         ( Syntax.get_contents_ty (get_expr_ty lhs),
           Syntax.get_contents_ty (get_expr_ty rhs) )
       with
      | TPtr lhs, TPtr rhs
        when lhs.pointee_depth <> rhs.pointee_depth
             || lhs.pointee_kind <> rhs.pointee_kind ->
          failwith "pointer type mismatch"
      | _ -> ());

      EAssign (get_expr_ty lhs, bin, lhs, rhs)
  | Syntax.EUnary (((Inc | Dec | Plus | Minus | BitNot | LogNot) as un), expr)
    ->
      let expr = type_expr is_unsafe env expr in
      EUnary (get_expr_ty expr, un, expr)
  | Syntax.EUnary (Ref, expr) -> (
      let expr = type_expr is_unsafe env expr in
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
            ( TPtr
                {
                  pointee_ownership = ownership;
                  pointee_ty = ty;
                  pointee_depth = depth;
                  pointee_kind = kind;
                  pointee_qual = qual;
                },
              Ref,
              expr )
      | _ -> failwith "not lvalue")
  | Syntax.EUnary (Deref, expr) -> (
      let expr = type_expr is_unsafe env expr in
      match Syntax.get_contents_ty (get_expr_ty expr) with
      | TPtr
          {
            pointee_ownership = ownership;
            pointee_ty = ty;
            pointee_depth = depth;
            pointee_kind = kind;
            pointee_qual = qual;
          } ->
          EUnary
            ( TVar
                {
                  ownership;
                  var_ty = ty;
                  var_depth = depth;
                  var_kind = kind;
                  var_qual = qual;
                },
              Deref,
              expr )
      | ty -> failwith ("type_expr is_unsafe env : deref " ^ show_ty ty))
  | Syntax.EUnary (Sizeof, expr) ->
      EUnary (TDeclSpec [ TsInt ], Sizeof, type_expr is_unsafe env expr)
  | Syntax.ESizeof ty -> ESizeof (TDeclSpec [ TsInt ], type_conv ty)
  | Syntax.EPostfix (expr, PCall args) -> (
      let expr = type_expr is_unsafe env expr in
      let args =
        List.map
          (function
            | Syntax.AExpr expr -> Syntax.AExpr (type_expr is_unsafe env expr)
            | Syntax.ADepth depth -> Syntax.ADepth depth)
          args
      in
      match
        Syntax.get_base_ty
          (Syntax.get_contents_ty (type_conv (get_expr_ty expr)))
      with
      | TFun (ret, _) -> EPostfix (ret, expr, PCall args)
      | _ ->
          print_endline (show_ty (type_conv (get_expr_ty expr)));
          failwith "type_expr is_unsafe env")
  | Syntax.EPostfix (expr, PIdx idx) -> (
      let expr = type_expr is_unsafe env expr in
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
              PIdx (type_expr is_unsafe env idx) )
      | ty -> failwith ("type_expr is_unsafe env : idx " ^ show_ty ty))
  | Syntax.EPostfix (expr, PDot name) -> (
      let expr = type_expr is_unsafe env expr in
      match get_expr_ty expr with
      | TVar
          {
            ownership;
            var_ty = TDeclSpec [ (TsStruct (id, largs) | TsUnion (id, largs)) ];
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, lparams, mems) | UnionDef (_, lparams, mems) ->
              let subst = unify lparams largs in
              EPostfix
                ( TVar
                    {
                      ownership;
                      var_ty = apply subst (type_conv (List.assoc name mems));
                      var_depth = depth;
                      var_kind = kind;
                      var_qual = qual;
                    },
                  expr,
                  PDot name )
          | _ -> failwith "type_expr is_unsafe env: dot")
      | TVar
          {
            ownership;
            var_ty = TDeclSpec [ (TsStructDef id | TsUnionDef id) ];
            var_depth = depth;
            var_kind = kind;
            var_qual = qual;
          } -> (
          match List.nth (List.rev !Env.program) id with
          | StructDef (_, [], mems) | UnionDef (_, [], mems) ->
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
          | _ -> failwith "type_expr is_unsafe env: dot")
      | _ -> failwith "type_expr is_unsafe env: dot")
  | Syntax.EPostfix (expr, PArrow name) -> (
      let expr = type_expr is_unsafe env expr in
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
          | TDeclSpec [ (TsStruct (id, largs) | TsUnion (id, largs)) ] -> (
              match List.nth (List.rev !Env.program) id with
              | StructDef (_, lparams, mems) | UnionDef (_, lparams, mems) ->
                  let subst = unify lparams largs in
                  EPostfix
                    ( TVar
                        {
                          ownership;
                          var_ty =
                            apply subst (type_conv (List.assoc name mems));
                          var_depth = depth;
                          var_kind = kind;
                          var_qual = qual;
                        },
                      expr,
                      PArrow name )
              | _ -> failwith "type_expr is_unsafe env: arrow")
          | TDeclSpec [ (TsStructDef id | TsUnionDef id) ] -> (
              match List.nth (List.rev !Env.program) id with
              | StructDef (_, [], mems) | UnionDef (_, [], mems) ->
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
              | _ -> failwith "type_expr is_unsafe env: arrow")
          | _ -> failwith "type_expr is_unsafe env: not a compound type")
      | _ ->
          print_endline (show_ty (Syntax.get_base_ty (get_expr_ty expr)));
          failwith "type_expr is_unsafe env: arrow")
  | Syntax.EPostfix (expr, ((PInc | PDec) as postfix)) ->
      EPostfix
        ( Syntax.get_contents_ty (get_expr_ty (type_expr is_unsafe env expr)),
          type_expr is_unsafe env expr,
          postfix )
  | Syntax.ECond (cond, lhs, rhs) ->
      ECond
        ( Syntax.get_contents_ty (get_expr_ty (type_expr is_unsafe env lhs)),
          type_expr is_unsafe env cond,
          type_expr is_unsafe env lhs,
          type_expr is_unsafe env rhs )
  | Syntax.ECast (ty, expr) -> ECast (type_conv ty, type_expr is_unsafe env expr)
  | Syntax.ECompoundLit (ty, init) ->
      ECompoundLit (type_conv ty, type_init is_unsafe env (type_conv ty) init)

and type_init is_unsafe env ty init =
  match init with
  | Syntax.IScal expr ->
      let rhs = type_expr is_unsafe env expr in
      (if not is_unsafe then
         match (ty, Syntax.get_contents_ty (get_expr_ty rhs)) with
         | ( TVar { var_depth = depth; _ },
             TPtr { pointee_ownership = ownership; _ } ) ->
             ownership := Moved depth
             (*print_endline ("ownership:" ^ show_ty (get_expr_ty rhs))*)
         | _ -> ());
      (match
         (Syntax.get_contents_ty ty, Syntax.get_contents_ty (get_expr_ty rhs))
       with
      | TPtr lhs, TPtr rhs
        when lhs.pointee_depth <> rhs.pointee_depth
             || lhs.pointee_kind <> rhs.pointee_kind ->
          print_endline (Syntax.show_ty_ ty);
          failwith "pointer type mismatch"
      | _ -> ());
      Syntax.IScal rhs
  | Syntax.IVect l -> (
      let loc = ref 0 in
      match ty with
      | Syntax.TArr (ty, _) ->
          let rec aux = function
            | [] -> []
            | (design, init) :: xs ->
                let ty, design = type_design is_unsafe env ty design loc in
                (design, type_init is_unsafe env ty init) :: aux xs
          in
          IVect (aux l)
      | Syntax.TDeclSpec [ (TsStruct (id, _) | TsStructDef id) ] ->
          let mems =
            match List.nth (List.rev !Env.program) id with
            | StructDef (_, _, mems) -> mems
            | _ -> failwith "type_init is_unsafe env"
          in
          let rec aux loc mems l =
            match (mems, l) with
            | _, [] -> []
            | [], _ -> failwith "type_init is_unsafe env: excess elements"
            | (_, memty) :: _, (design, init) :: xs ->
                let memty, design =
                  match design with
                  | Syntax.Dnone ->
                      loc := !loc + 1;
                      (type_conv memty, Syntax.Dnone)
                  | _ -> type_design is_unsafe env (type_conv ty) design loc
                in
                let rec remain_mems mems x =
                  match (mems, x) with
                  | mems, 0 -> mems
                  | [], _ ->
                      failwith "type_init is_unsafe env error: excess elements"
                  | _ :: mems, x -> remain_mems mems (x - 1)
                in
                (design, type_init is_unsafe env memty init)
                :: aux loc (remain_mems mems !loc) xs
          in
          IVect (aux loc mems l)
      | _ -> failwith "type_init is_unsafe env: invalid type or initializer")

and type_design is_unsafe env ty design loc =
  match (ty, design) with
  | Syntax.TArr (ty, _), DIdx (expr, design) ->
      let ty, design = type_design is_unsafe env ty design loc in
      (ty, DIdx (type_expr is_unsafe env expr, design))
  | Syntax.TDeclSpec [ TsStruct (id, _) ], DField (name, design) ->
      let mems =
        match List.nth (List.rev !Env.program) id with
        | StructDef (_, _, mems) -> mems
        | _ -> failwith "type_init is_unsafe env"
      in
      let ty =
        try List.assoc name mems
        with _ -> failwith "type_design is_unsafe env"
      in
      let l = List.init (List.length mems) (fun x -> x) in
      let l = List.map2 (fun (name, _) loc -> (name, loc)) mems l in
      loc := List.assoc name l;
      let ty, design = type_design is_unsafe env ty design (ref 0) in
      (ty, DField (name, design))
  | ty, Dnone -> (type_conv ty, Dnone)
  | _ ->
      failwith
        ("type_design is_unsafe env" ^ Syntax.show_ty_ ty
       ^ Syntax.show_desig design)

let rec type_stmt is_unsafe env =
  let open Syntax in
  function
  | SDef l ->
      let f id =
        match List.nth (List.rev !Env.program) id with
        | VarDef ((_, ty), init, depth, ownership) ->
            let ty =
              Syntax.TVar
                {
                  ownership;
                  var_ty = type_conv ty;
                  var_depth = depth;
                  var_kind = Auto;
                  var_qual = [];
                }
            in
            ignore (type_init is_unsafe env ty init)
        | _ -> ()
      in
      List.iter f l;
      SDef l
  | SUnsafe stmts -> SUnsafe (List.map (type_stmt true env) stmts)
  | SStmts (d, stmts) ->
      SStmts (d, List.map (type_stmt is_unsafe (d :: env)) stmts)
  | SWhile (expr, stmt) ->
      SWhile (type_expr is_unsafe env expr, type_stmt is_unsafe env stmt)
  | SDoWhile (stmt, expr) ->
      SDoWhile (type_stmt is_unsafe env stmt, type_expr is_unsafe env expr)
  | SFor (stmt1, expr1, expr2, stmt2) ->
      SFor
        ( type_stmt is_unsafe env stmt1,
          Option.map (type_expr is_unsafe env) expr1,
          Option.map (type_expr is_unsafe env) expr2,
          type_stmt is_unsafe env stmt2 )
  | SIfElse (expr, stmt1, stmt2) ->
      SIfElse
        ( type_expr is_unsafe env expr,
          type_stmt is_unsafe env stmt1,
          type_stmt is_unsafe env stmt2 )
  | SReturn expr -> SReturn (Option.map (type_expr is_unsafe env) expr)
  | SLabel (name, stmt) -> SLabel (name, type_stmt is_unsafe env stmt)
  | SGoto name -> SGoto name
  | SContinue -> SContinue
  | SBreak -> SBreak
  | SSwitch (expr, stmt) ->
      SSwitch (type_expr is_unsafe env expr, type_stmt is_unsafe env stmt)
  | SCase (expr, stmts) ->
      SCase
        (type_expr is_unsafe env expr, List.map (type_stmt is_unsafe env) stmts)
  | SDefault stmts -> SDefault (List.map (type_stmt is_unsafe env) stmts)
  | SExpr expr -> SExpr (Option.map (type_expr is_unsafe env) expr)

let check_droped_params decls =
  let open Syntax in
  let rec check n name = function
    | TPtr
        {
          pointee_ownership = ownership;
          pointee_ty = ty;
          pointee_qual = qual;
          _;
        } -> (
        match !ownership with
        | Has -> check (n + 1) name ty
        | _ when List.mem Drop qual -> ()
        | _ -> failwith (String.make n '*' ^ name ^ " dropped"))
    | _ -> ()
  in
  List.iter (fun (name, ty) -> check 0 name ty) decls

let rec type_program =
  let open Syntax in
  function
  | [] -> []
  | Block (d, n) :: xs -> Block (d, n) :: type_program xs
  | Kind n :: xs -> Kind n :: type_program xs
  | Param ((n, ty), d, own) :: xs ->
      check_ty ty;
      Param ((n, type_conv ty), d, own) :: type_program xs
  | Decl ((n, ty), d, own) :: xs ->
      check_ty ty;
      Decl ((n, type_conv ty), d, own) :: type_program xs
  | GDecl (n, ty) :: xs ->
      check_ty ty;
      GDecl (n, type_conv ty) :: type_program xs
  | LDecl (lparams, (n, ty)) :: xs ->
      check_ty ty;
      LDecl (lparams, (n, type_conv ty)) :: type_program xs
  | StructDecl (n, lp) :: xs -> StructDecl (n, lp) :: type_program xs
  | UnionDecl (n, lp) :: xs -> UnionDecl (n, lp) :: type_program xs
  | EnumDecl n :: xs -> EnumDecl n :: type_program xs
  | VarDef ((n, ty), init, d, own) :: xs ->
      check_ty ty;
      VarDef ((n, type_conv ty), type_init false [] (type_conv ty) init, d, own)
      :: type_program xs
  | GVarDef ((n, ty), init) :: xs ->
      check_ty ty;
      GVarDef ((n, type_conv ty), type_init false [] (type_conv ty) init)
      :: type_program xs
  | StructDef (n, lp, l) :: xs ->
      StructDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | UnionDef (n, lp, l) :: xs ->
      UnionDef (n, lp, List.map (fun (n, ty) -> (n, type_conv ty)) l)
      :: type_program xs
  | EnumDef (n, l) :: xs -> EnumDef (n, l) :: type_program xs
  | FunctionDef ((n, ty), decls, stmt) :: xs ->
      let item =
        FunctionDef
          ( (n, type_conv ty),
            List.map (fun (n, ty) -> (n, type_conv ty)) decls,
            type_stmt false [] stmt )
      in
      check_droped_params decls;
      item :: type_program xs
  | LFunctionDef (lparams, (n, ty), decls, stmt) :: xs ->
      let item =
        LFunctionDef
          ( lparams,
            (n, type_conv ty),
            List.map (fun (n, ty) -> (n, type_conv ty)) decls,
            type_stmt false (Syntax.filter_depth lparams) stmt )
      in
      check_droped_params decls;
      item :: type_program xs
