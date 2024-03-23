exception ASTError of string

let raise exn =
  match exn with
  (*| ASTError msg -> Printf.printf "%s\n" msg;raise exn*)
  | _ -> raise exn

let spr fmt s = Printf.sprintf fmt s

type value =
  | VChar of string
  | VInt of string
  | VFloat of string
  | VStr of string
  | VNull
[@@deriving show]

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BitAnd
  | BitXor
  | BitOr
  | LogAnd
  | LogOr
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Comma
[@@deriving show]

type unary = Inc | Dec | Plus | Minus | BitNot | LogNot | Ref | Deref | Sizeof
[@@deriving show]

type 'expr item =
  | Block of string * int
  | Kind of string
  | Param of 'expr decl * depth * ownership ref
  | Decl of 'expr decl * depth * ownership ref
  | GDecl of 'expr decl
  | LDecl of lparam list * 'expr decl
  | StructDecl of string * lparam list
  | UnionDecl of string * lparam list
  | EnumDecl of string
  | VarDef of 'expr decl * 'expr init * depth * ownership ref
  | GVarDef of 'expr decl * 'expr init
  | StructDef of string * lparam list * 'expr decl list
  | UnionDef of string * lparam list * 'expr decl list
  | EnumDef of string * (string * int) list
  | FunctionDef of 'expr decl * 'expr decl list * 'expr stmt
  | LFunctionDef of lparam list * 'expr decl * 'expr decl list * 'expr stmt
[@@deriving show]

and id = int [@@deriving show]
and id_list = int list [@@deriving show]
and id_list_list = int list list [@@deriving show]
and program = expr item list [@@deriving show]
and programi = (id * expr item) list [@@deriving show]
and ty_ = expr ty [@@deriving show]
and desig = expr design [@@deriving show]
and item_ = expr item [@@deriving show]

and expr =
  | EConst of value
  | EVar of id * lparam list
  | EBinary of binary * expr * expr
  | EAssign of binary option * expr * expr
  | EUnary of unary * expr
  | ESizeof of expr ty
  | EPostfix of expr * expr postfix
  | ECond of expr * expr * expr
  | ECast of expr ty * expr
  | ECompoundLit of expr ty * expr init
[@@deriving show]

and 'expr postfix =
  | PCall of 'expr arg list
  | PIdx of 'expr
  | PDot of string
  | PArrow of string
  | PInc
  | PDec
[@@deriving show]

and 'expr arg = AExpr of 'expr | ADepth of depth

and 'expr init = IScal of 'expr | IVect of ('expr design * 'expr init) list
[@@deriving show]

and 'expr design =
  | Dnone
  | DIdx of 'expr * 'expr design
  | DField of string * 'expr design
[@@deriving show]

and 'expr stmt =
  | SDef of id list
  | SUnsafe of 'expr stmt list
  | SStmts of depth * 'expr stmt list
  | SWhile of 'expr * 'expr stmt
  | SDoWhile of 'expr stmt * 'expr
  | SFor of 'expr stmt * 'expr option * 'expr option * 'expr stmt
  | SIfElse of 'expr * 'expr stmt * 'expr stmt
  | SReturn of 'expr option
  | SLabel of string * 'expr stmt
  | SGoto of string
  | SContinue
  | SBreak
  | SSwitch of 'expr * 'expr stmt
  | SCase of 'expr * 'expr stmt list
  | SDefault of 'expr stmt list
  | SExpr of 'expr option
[@@deriving show]

and 'expr ty =
  | TVar of 'expr var
  | TFun of 'expr ty * 'expr decl list
  | TPtr of 'expr pointer
  | TArr of 'expr ty * 'expr
  | TDeclSpec of ds list
  | TBlock
  | TVarArgs
[@@deriving show]

and 'expr decl = string * 'expr ty [@@deriving show]

and ds =
  | TsInt
  | TsShort
  | TsLong
  | TsChar
  | TsSigned
  | TsUnsigned
  | TsFloat
  | TsDouble
  | TsVoid
  | TsStruct of int * lparam list
  | TsUnion of int * lparam list
  | TsStructDef of int
  | TsUnionDef of int
  | TsTypedef of int
  | ScsTypedef
  | ScsExtern
  | ScsStatic
  | ScsAuto
  | ScsRegister
  | TqConst
  | TqVolatile
  | FsInline
  | FsNoreturn
[@@deriving show]

and depth = Depth of string * int | Global | Decayed [@@deriving show]
and kind = Auto | Dyn | Static | User of string | Unknown [@@deriving show]
and lparam = LBlock of depth | LKind of kind [@@deriving show]
and depths = depth list [@@deriving show]

and 'expr pointer = {
  pointee_ownership : ownership ref;
  pointee_ty : 'expr ty;
  pointee_depth : depth;
  pointee_kind : kind;
  pointee_qual : qualifier list;
}
[@@deriving show]

and 'expr var = {
  ownership : ownership ref;
  var_ty : 'expr ty;
  var_depth : depth;
  var_kind : kind;
  var_qual : qualifier list;
}
[@@deriving show]

and qualifier = Const | Volatile | Drop [@@deriving show]
and ownership = Has | Moved of depth | Dropped [@@deriving show]

let rec get_declspec = function
  | TVar { var_ty = ty; _ }
  | TFun (ty, _)
  | TPtr { pointee_ty = ty; _ }
  | TArr (ty, _) ->
      get_declspec ty
  | TDeclSpec l -> l
  | _ -> failwith "gen_declspec"

let get_base_ty = function
  | TPtr { pointee_ty = ty; _ } | TArr (ty, _) -> ty
  | _ -> failwith "get_base_ty"

let get_contents_ty = function TVar { var_ty = ty; _ } | ty -> ty

let filter_depth l =
  List.filter_map (function LBlock depth -> Some depth | _ -> None) l
