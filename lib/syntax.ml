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

type unary = Plus | Minus | BitNot | LogNot | Ref | Deref | Sizeof
[@@deriving show]

type 'expr item =
  | Depth of string * int
  | Kind of string
  | Decl of 'expr decl
  | GDecl of 'expr decl
  | LDecl of string list * string list * 'expr decl
  | StructDecl of string
  | UnionDecl of string
  | EnumDecl of string
  | VarDef of 'expr decl * 'expr init
  | GVarDef of 'expr decl * 'expr init
  | StructDef of string * 'expr decl list
  | UnionDef of string * 'expr decl list
  | EnumDef of string * (string * int) list
  | FunctionDef of 'expr decl * 'expr stmt
  | LFunctionDef of string list * string list * 'expr decl * 'expr stmt
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
  | EVar of id
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
  | PCall of 'expr list
  | PIdx of 'expr
  | PDot of string
  | PArrow of string
  | PInc
  | PDec
[@@deriving show]

and 'expr init = IScal of 'expr | IVect of ('expr design * 'expr init) list
[@@deriving show]

and 'expr design =
  | Dnone
  | DIdx of 'expr * 'expr design
  | DField of string * 'expr design
[@@deriving show]

and 'expr stmt =
  | SDef of id list
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
  | SSwitch of 'expr * 'expr stmt list
  | SCase of 'expr * 'expr stmt list
  | SDefault of 'expr stmt list
  | SExpr of 'expr option
[@@deriving show]

and 'expr ty =
  | TFun of 'expr ty * 'expr decl list
  | TPtr of 'expr ty
  | TConstPtr of 'expr ty
  | TArr of 'expr ty * 'expr
  | TDeclSpec of ds list
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
  | TsStruct of int
  | TsUnion of int
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

and depth = Local of string * int | Static

let rec get_declspec = function
  | TFun (ty, _) | TConstPtr ty | TPtr ty | TArr (ty, _) -> get_declspec ty
  | TDeclSpec l -> l

let get_base_ty = function
  | TConstPtr ty | TPtr ty | TArr (ty, _) -> ty
  | _ -> failwith "get_base_ty"
