type expr =
  | EConst of expr Syntax.ty * Syntax.value
  | EVar of expr Syntax.ty * Syntax.id
  | EBinary of expr Syntax.ty * Syntax.binary * expr * expr
  | EAssign of expr Syntax.ty * expr * expr
  | EUnary of expr Syntax.ty * Syntax.unary * expr
  | ESizeof of expr Syntax.ty * expr Syntax.ty
  | EPostfix of expr Syntax.ty * expr * expr Syntax.postfix
  | ECond of expr Syntax.ty * expr * expr * expr
  | ECSyntax of expr Syntax.ty * expr
  | ECompoundLit of expr Syntax.ty * expr Syntax.init
[@@deriving show]
