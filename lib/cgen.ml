open Syntax
(* open Typing *)

let rec gen_ty str = function
  | TConstPtr ((TArr _ | TFun _) as ty) ->
      gen_ty ("(" ^ "*const " ^ str ^ ")") ty
  | TConstPtr ty -> gen_ty ("*const " ^ str) ty
  | TPtr ((TArr _ | TFun _) as ty) -> gen_ty ("(" ^ "*" ^ str ^ ")") ty
  | TPtr ty -> gen_ty ("*" ^ str) ty
  | TArr (ty, _) -> gen_ty (str ^ "[" ^ "]") ty
  | TFun (ty, _) -> gen_ty (str ^ "(" ^ ")") ty
  | TDeclSpec _ -> "void " ^ str
