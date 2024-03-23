%{
open Syntax 
open Env 

type declarator =
| DeclPtr of declarator * depth * kind * qualifier list
| DeclIdent of string
| DeclArr of declarator * expr
| DeclFun of declarator * expr decl list

let make_decl ty d = 
  let name = ref "" in
  let rec aux ty = function
    | DeclPtr (d, dep, kind, qual) -> aux (TPtr {
                  pointee_ownership = ref Has;
                  pointee_ty = ty;
                  pointee_depth = dep;
                  pointee_kind = kind;
                  pointee_qual = qual
                }) d 
    | DeclIdent n -> name := n; ty 
    | DeclArr(d,sz) -> aux (TArr(ty,sz)) d 
    | DeclFun(d,dl) -> aux (TFun(ty,dl)) d
  in
  (!name,aux ty d)

let make_decls ty dl =
  List.map (fun d -> make_decl ty d) dl

let make_decls_with_init ty init_decl_list depth =
  List.map (function 
    | (d,Some init) -> push_def (VarDef(make_decl ty d,init, depth, ref Has))
    | (d,None) -> push_def (Decl (make_decl ty d, depth, ref Has))
  ) init_decl_list

let make_gdecls_with_init ty init_decl_list =
  List.map (function 
    | (d,Some init) -> push_def (GVarDef(make_decl ty d,init))
    | (d,None) -> push_def (GDecl (make_decl ty d))
  ) init_decl_list

let conv_ident = function
  | Some s -> s 
  | None -> ""

%}
%token LPAREN "(" RPAREN ")" LBRACKET "[" RBRACKET "]" LBRACE "{" RBRACE "}" DOT "." COMMA ","
%token AND "&" STAR "*" PLUS "+" MINUS "-" NOT "~" BANG "!" DIV "/" MOD "%" LT "<" GT ">" HAT "^" OR "|" 
%token COLON ":" QUESTION "?" SEMI ";" EQ "=" INLINE NORETURN
%token SIZEOF EOF
%token ARROW "->" INC "++" DEC "--" LSHIFT "<<" RSHIFT ">>" LE "<=" GE ">=" EQEQ "==" NE "!=" ELLIPSIS "..."
%token ANDAND "&&" OROR "||" MUL_EQ "*=" DIV_EQ "/=" MOD_EQ "%=" ADD_EQ "+="
%token SUB_EQ "-=" LSHIFT_EQ "<<=" RSHIFT_EQ ">>=" AND_EQ "&="
%token XOR_EQ "^=" OR_EQ "|="
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token TCHAR TSHORT TINT TLONG TSIGNED TUNSIGNED TFLOAT TDOUBLE CONST VOLATILE TVOID
%token STRUCT UNION ENUM UNSAFE
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN
%token DYN DROP USING DEPTH KIND LIFETIME NULL


%token <string> CHAR INT
%token <string> FLOAT
%token<string> STR
%token<string> ID LID TYPE_ID DEPTH_ID KIND_ID

%nonassoc NO_ELSE
%nonassoc ELSE 

%type<id list> translation_unit
%start translation_unit
%%

translation_unit:
| list(external_decl) EOF                 { List.flatten $1 }

ident:
| ID                                      { $1 }
| LID                                     { $1 }
| DEPTH_ID                                { $1 }
| KIND_ID                                 { $1 }
| TYPE_ID                                 { $1 }

larg:
| DEPTH_ID                                { LBlock (Depth (fst (get_depth $1), snd (get_depth $1))) }
| depth_qualifer                          { LKind $1  }

primary_expr:
| ID                                      { EVar (lookup_var $1, []) }
| LID LT separated_list(",", larg) GT     { EVar (lookup_var $1, $3) }
| CHAR                                    { EConst (VChar $1) }
| INT                                     { EConst (VInt $1) }
| FLOAT                                   { EConst (VFloat $1) }
| STR                                     { EConst (VStr $1) }
| NULL                                    { EConst VNull }
| "(" expr ")"                            { $2 }

postfix_expr:
| primary_expr                            { $1 }
| postfix_expr "[" expr "]"               { EPostfix($1,PIdx $3) }
| postfix_expr "(" argument_expr_list? ")"
                                          { match $3 with
                                            | Some l -> EPostfix($1,PCall l)
                                            | None -> EPostfix($1,PCall []) }
| postfix_expr "." ident                  { EPostfix($1,PDot $3) }
| postfix_expr "->" ident                 { EPostfix($1,PArrow $3) }
| postfix_expr "++"                       { EPostfix($1,PInc) }
| postfix_expr "--"                       { EPostfix($1,PDec) }
| "(" type_name ")" "{" init_list ","? "}"
                                          { ECompoundLit($2,IVect $5) }

argument_expr_list:
| DEPTH_ID                                { [ADepth (Depth(fst (get_depth $1), snd (get_depth $1)))] }
| STATIC                                  { [ADepth Global] }
| assignment_expr                         { [AExpr $1] }
| argument_expr_list "," assignment_expr  { $1 @ [AExpr $3] }

unary_expr:
| postfix_expr                            { $1 }
| "++" unary_expr                         { EUnary(Inc,$2) }
| "--" unary_expr                         { EUnary(Dec,$2) }
| "&" cast_expr                           { EUnary(Ref,$2) }
| "*" cast_expr                           { EUnary(Deref,$2) }
| "+" cast_expr                           { EUnary(Plus,$2) }
| "-" cast_expr                           { EUnary(Minus,$2) }
| "~" cast_expr                           { EUnary(BitNot,$2) }
| "!" cast_expr                           { EUnary(LogNot,$2) }
| SIZEOF unary_expr                       { EUnary(Sizeof,$2) }
| SIZEOF "(" type_name ")"                { ESizeof($3) }


cast_expr:
| unary_expr                              { $1 }
| "(" type_name ")" cast_expr             { ECast($2,$4) }

multiplicative_expr:
| cast_expr                               { $1 }
| multiplicative_expr "*" cast_expr       { EBinary(Mul,$1,$3) }
| multiplicative_expr "/" cast_expr       { EBinary(Div,$1,$3) }
| multiplicative_expr "%" cast_expr       { EBinary(Mod,$1,$3) }

additive_expr:
| multiplicative_expr                     { $1 }
| additive_expr "+" multiplicative_expr   { EBinary(Add,$1,$3) }
| additive_expr "-" multiplicative_expr   { EBinary(Sub,$1,$3) }

shift_expr:
| additive_expr                           { $1 }
| shift_expr "<<" additive_expr           { EBinary(LShift,$1,$3) }
| shift_expr ">>" additive_expr           { EBinary(RShift,$1,$3) }

relational_expr:
| shift_expr                              { $1 }
| relational_expr "<" shift_expr          { EBinary(Lt,$1,$3) }
| relational_expr ">" shift_expr          { EBinary(Gt,$1,$3) }
| relational_expr "<=" shift_expr         { EBinary(Le,$1,$3) }
| relational_expr ">=" shift_expr         { EBinary(Ge,$1,$3) }

equality_expr:
| relational_expr                         { $1 }
| equality_expr "==" relational_expr      { EBinary(Eq,$1,$3) }
| equality_expr "!=" relational_expr      { EBinary(Ne,$1,$3) }

and_expr:
| equality_expr                           { $1 }
| and_expr "&" equality_expr              { EBinary(BitAnd,$1,$3) }

exclusive_or_expr:
| and_expr                                { $1 }
| exclusive_or_expr "^" and_expr          { EBinary(BitXor,$1,$3) }

inclusive_or_expr:
| exclusive_or_expr                       { $1 }
| inclusive_or_expr "|" exclusive_or_expr { EBinary(BitOr,$1,$3) }

logical_and_expr:
| inclusive_or_expr                       { $1 }
| logical_and_expr "&&" inclusive_or_expr { EBinary(LogAnd,$1,$3) }

logical_or_expr:
| logical_and_expr                        { $1 }
| logical_or_expr "||" logical_and_expr   { EBinary(LogOr,$1,$3) }

conditional_expr:
| logical_or_expr                         { $1 }
| logical_or_expr "?" expr ":" conditional_expr
                                          { ECond($1,$3,$5) }

assignment_expr:
| conditional_expr                        { $1 }
| unary_expr "=" assignment_expr          { EAssign(None,$1,$3) }
| unary_expr "*=" assignment_expr         { EAssign(Some Mul,$1,$3) }
| unary_expr "/=" assignment_expr         { EAssign(Some Div,$1,$3) }
| unary_expr "%=" assignment_expr         { EAssign(Some Mod,$1,$3) }
| unary_expr "+=" assignment_expr         { EAssign(Some Add,$1,$3) }
| unary_expr "-=" assignment_expr         { EAssign(Some Sub,$1,$3) }
| unary_expr "<<=" assignment_expr        { EAssign(Some LShift,$1,$3) }
| unary_expr ">>=" assignment_expr        { EAssign(Some RShift,$1,$3) }
| unary_expr "&=" assignment_expr         { EAssign(Some BitAnd,$1,$3) }
| unary_expr "^=" assignment_expr         { EAssign(Some BitXor,$1,$3) }
| unary_expr "|=" assignment_expr         { EAssign(Some BitOr,$1,$3) }

expr:
| assignment_expr                         { $1 }
| expr "," assignment_expr                { EBinary(Comma,$1,$3) }

constant_expr:
| conditional_expr                        { $1 }

lifetime_declaration:
| LIFETIME LT separated_list(",", lparam) GT { in_lparams := true; $3 }

lparam:
| DEPTH ident                             
                                          { let depth = get_curr_depth () in
                                            ignore(push_lparam_depth (Block ($2, depth))); LBlock (Depth ($2, depth)) }
| KIND ident                              { ignore(push_lparam (Kind $2)); LKind (User $2) }

decl:
| decl_specs                          { make_decls_with_init $1 [((DeclIdent ""), None)] (lookup_last_depth ()) }
| decl_specs enter_scope_first init_declarator_list leave_scope_last    { make_decls_with_init $1 $3 (lookup_last_depth ()) }

gdecl:
| decl_specs                          { [push_def (GDecl (make_decl $1 (DeclIdent "")))] }
| decl_specs enter_scope_first init_declarator_list leave_scope_last    { make_gdecls_with_init $1 $3 }

decl_spec:
| storage_class_spec                      { [$1] }
| type_qual                               { [$1] }
| function_spec                           { [$1] }
| type_spec                               { [$1] }

decl_specs:
| decl_specs_sub                          { TDeclSpec $1 }
| TYPE_ID                                 { TDeclSpec [TsTypedef (Option.get (lookup_typedef1 $1))] }

decl_specs_sub:
| decl_spec                               { $1 }
| decl_specs_sub decl_spec                { $1 @ $2 }

init_declarator_list:
| init_declarator                         { [$1] }
| init_declarator_list "," init_declarator
                                          { $1 @ [$3] }

init_declarator:
| declarator                              { ($1,None) }
| declarator "=" init                     { ($1,Some $3) }

storage_class_spec:
| TYPEDEF                                 { ScsTypedef }
| EXTERN                                  { ScsExtern }
| STATIC                                  { ScsStatic }
| AUTO                                    { ScsAuto }
| REGISTER                                { ScsRegister }

type_spec:
| TVOID                                   { TsVoid }
| TCHAR                                   { TsChar }
| TSHORT                                  { TsShort }
| TINT                                    { TsInt }
| TLONG                                   { TsLong }
| TFLOAT                                  { TsFloat }
| TDOUBLE                                 { TsDouble }
| TSIGNED                                 { TsSigned }
| TUNSIGNED                               { TsUnsigned }
| struct_or_union_spec                    { $1 }
| enum_spec                               { TsInt }


spec_qual_list:
| spec_qual_list_sub                      { $1 }

spec_qual_list_sub:
| type_spec                               { $1::[] }
| type_spec spec_qual_list_sub            { $1::$2 }
| type_qual spec_qual_list_sub            { $1::$2 }

type_qual:
| CONST                                   { TqConst }
| VOLATILE                                { TqVolatile }

function_spec:
| INLINE                                  { FsInline }
| NORETURN                                { FsNoreturn }


struct_or_union_spec:
| STRUCT ident? "{" list(struct_decl) "}" { make_structdef
                                              (conv_ident $2)
                                              []
                                              (List.flatten $4) }
| STRUCT ident                            { make_structdecl $2 [] } 
| STRUCT ident  LT separated_list(",", larg) GT { make_structdecl $2 $4 }
| UNION ident? "{" list(struct_decl) "}"  { make_uniondef
                                              (conv_ident $2)
                                              []
                                              (List.flatten $4) }
| UNION ident                             { make_uniondecl $2 [] }
| UNION ident  LT separated_list(",", larg) GT { make_uniondecl $2 $4 }

lstruct_decl:
| lifetime_declaration STRUCT ident? "{" list(struct_decl) "}" enter_scope_first leave_scope_last                         
                                            { GDecl (make_decl (TDeclSpec [make_structdef
                                              (conv_ident $3)
                                              $1
                                              (List.flatten $5)]) (DeclIdent "")) }
| lifetime_declaration UNION ident? "{" list(struct_decl) "}" enter_scope_first leave_scope_last                         
                                            { GDecl (make_decl (TDeclSpec [make_uniondef
                                              (conv_ident $3)
                                              $1
                                              (List.flatten $5)]) (DeclIdent "")) }
| lifetime_declaration STRUCT ident                         
                                            { GDecl (make_decl (TDeclSpec [make_structdecl
                                              $3
                                              $1]) (DeclIdent "")) }
| lifetime_declaration UNION ident                         
                                            { GDecl (make_decl (TDeclSpec [make_uniondecl
                                              $3
                                              $1]) (DeclIdent "")) }
struct_decl:
| spec_qual_list struct_declarator_list? ";"
                                          { match $2 with
                                            | Some dl ->
                                                make_decls (TDeclSpec $1) dl
                                            | None -> failwith "not impl" }
struct_declarator_list:
| struct_declarator                       { [$1] }
| struct_declarator_list "," struct_declarator
                                          { $1 @ [$3] }

struct_declarator:
| declarator                              { $1 }

enum_spec:
| ENUM ident? "{" enum_list ","? "}"      { }
| ENUM ident                              { }

enum_list:
| enum                                    { }
| enum_list "," enum                      { }

enum:
| enum_const                              { }
| enum_const "=" constant_expr            { }

enum_const:
| ident                                   { }

declarator:
| pointer declarator                      { DeclPtr ($2, fst (fst $1), snd (fst $1), snd $1) }
| direct_declarator                       { $1 }

direct_declarator:
| ident                                   { DeclIdent $1 }
| "(" id_declarator ")"                   { $2 }
| direct_declarator "[" constant_expr "]" { DeclArr($1,$3) }
| direct_declarator "(" parameter_type_list ")"
                                       { DeclFun($1,$3) }

id_declarator:
| pointer declarator                      { DeclPtr ($2, fst (fst $1), snd (fst $1), snd $1) }
| direct_id_declarator                    { $1 }

direct_id_declarator:
| ID                                      { DeclIdent $1 }
| LID                                     { DeclIdent $1 }
| DEPTH_ID                                { DeclIdent $1 }
| KIND_ID                                 { DeclIdent $1 }
| "(" id_declarator ")"                   { $2 }
| direct_id_declarator "[" constant_expr "]" { DeclArr($1,$3) }
| direct_id_declarator "(" parameter_type_list ")"
                                          { DeclFun($1,$3) }

depth_qualifer:
| STATIC                                  { Static }
| AUTO                                    { Auto }
| DYN                                     { Dyn }
| KIND_ID                                 { User $1 }

pointer_qual:
| CONST                                   { Const }
| VOLATILE                                { Volatile }
| DROP                                    { Drop }

pointer:
| "*" list(pointer_qual)                        { ((Global, Static), $2) }
| DEPTH_ID "*" list(pointer_qual)               { ((Depth (fst (get_depth $1), snd (get_depth $1)), Auto), $3) }
| DEPTH_ID depth_qualifer "*" list(pointer_qual) { ((Depth (fst (get_depth $1), snd (get_depth $1)), $2), $4) }
| DYN "*" list(pointer_qual)                    { ((Global, Dyn), $3) }

parameter_type_list:
|                                         { [] }
| parameter_list option("," "..." {})     { $1 }

parameter_list:
| parameter_decl                          { $1 }
| parameter_list "," parameter_decl       { $1 @ $3 }

parameter_decl:
| DEPTH DEPTH_ID                          { [($2, TBlock)] }
| decl_specs declarator                   { let decl = make_decl $1 $2 in push_decl_in_params decl;
                                            [make_decl $1 $2] }
| decl_specs abstract_declarator?         { match $2 with
                                            | Some d -> [make_decl $1 d]
                                            | None ->
                                                [make_decl $1 (DeclIdent "")] }

abstract_declarator:
| pointer                                 { DeclPtr (DeclIdent "", fst (fst $1), snd (fst $1), snd $1) }
| pointer abstract_declarator             { DeclPtr ($2, fst (fst $1), snd (fst $1), snd $1) }
| direct_abstract_declarator              { $1 }

direct_abstract_declarator:
| "(" abstract_declarator ")"             { $2 }
| "[" constant_expr "]"                   { DeclArr(DeclIdent "",$2) }
| "(" parameter_type_list ")"             { DeclFun(DeclIdent "",$2) }
| direct_abstract_declarator "[" constant_expr "]"
                                          { DeclArr($1,$3) }
| direct_abstract_declarator "(" parameter_type_list ")"
                                          { DeclFun($1,$3) }



type_name:
| spec_qual_list                          { TDeclSpec $1 }
| spec_qual_list abstract_declarator      { TDeclSpec [] }

init:
| assignment_expr                         { IScal $1 }
| "{" init_list ","? "}"                  { IVect $2 }

init_list:
| init                                    { [(Dnone,$1)] }
| desig init                              { [($1,$2)] }
| init_list "," init                      { $1 @ [(Dnone,$3)] }
| init_list "," desig init                { $1 @ [($3,$4)] }

desig:
| designator_list "="                     { $1 }

designator_list:
| "[" constant_expr "]"                   { DIdx($2,Dnone) }
| "." ident                               { DField($2,Dnone) }
| "[" constant_expr "]" designator_list   { DIdx($2,$4) } 
| "." ident designator_list               { DField($2,$3) }

enter_scope:
|                                         { enter_scope () }

enter_scope_first:
|                                         { enter_scope_first () }


leave_scope:
|                                         { leave_scope () }

leave_scope_last:
|                                         { leave_scope_last () }

item:
| decl ";"                                { SDef($1) }
| stmt                                    { $1 }
| compound_stmt                           { $1 }

stmt:
| unsafe_stmt                             { $1 }
| labeled_stmt                            { $1 }
| expr? ";"                               { SExpr $1 }
| selection_stmt_1                        { $1 }
| selection_stmt_2                        { $1 }
| iteration_stmt                          { $1 }
| jump_stmt                               { $1 }

unsafe_stmt:
| UNSAFE  "{" list(item) "}"              { SUnsafe($3) }

labeled_stmt:
| ident ":" item                          { SLabel($1,$3) }

case_or_default:
| CASE conditional_expr ":" list(item)    { SCase ($2,$4) }
| DEFAULT ":" list(item)                  { SDefault ($3) }

using_depth:
| USING ident                             { ignore (push_def (Block ($2, get_curr_depth ()))); push_params $2 (get_curr_depth ()); ($2, get_curr_depth ()) }

no_depth:
|                                         { ignore (push_def (Block ("", get_curr_depth ()))); push_params "" (get_curr_depth ()); ("", get_curr_depth ()) }

compound_stmt:
| enter_scope  no_depth "{" list(item) "}" leave_scope
                                          { SStmts(Depth(fst $2, snd $2), $4) }
| enter_scope using_depth "{" list(item) "}" leave_scope
                                          { SStmts(Depth(fst $2, snd $2), $4) }

selection_stmt_1:
| IF "(" expr ")" item %prec NO_ELSE      { SIfElse($3,$5,SStmts (Depth("", get_curr_depth ()), [])) }
| IF "(" expr ")" item ELSE item          { SIfElse($3,$5,$7) }

selection_stmt_2:
| SWITCH "(" expr ")" enter_scope no_depth "{" list(case_or_default) "}" leave_scope
                                          { SSwitch($3,SStmts(Depth(fst $6, snd $6), $8)) }
| SWITCH "(" expr ")" enter_scope using_depth "{" list(case_or_default) "}" leave_scope
                                          { SSwitch($3,SStmts(Depth(fst $6, snd $6), $8)) }

iteration_stmt:
| WHILE "(" expr ")" item                 { SWhile($3,$5) }
| DO item WHILE "(" expr ")"              { SDoWhile($2,$5) }
| FOR "(" expr? ";" expr? ";" expr? ")" item
                                          { SFor(SExpr $3,$5,$7,$9) }
| FOR "(" enter_scope decl ";" expr? ";" expr? ")" decl ";" leave_scope
                                          { SFor(SDef $4,$6,$8,SDef $10) } 
| FOR "(" enter_scope decl ";" expr? ";" expr? ")" stmt leave_scope
                                          { SFor(SDef $4,$6,$8,$10) }
| FOR "(" enter_scope decl ";" expr? ";" expr? ")" no_depth "{" list(item) "}" leave_scope
                                          { SFor(SDef $4,$6,$8,SStmts (Depth(fst $10, snd $10), $12)) }
| FOR "(" enter_scope decl ";" expr? ";" expr? ")" using_depth "{" list(item) "}" leave_scope
                                          { SFor(SDef $4,$6,$8,SStmts (Depth(fst $10, snd $10), $12)) }                                      
jump_stmt:
| GOTO ident ";"                          { SGoto $2 }
| CONTINUE ";"                            { SContinue }
| BREAK ";"                               { SBreak }
| RETURN expr? ";"                        { SReturn $2 }

external_decl:
| function_def                            { [push_def $1] }
| gdecl ";"                               { $1 }
| ldecl ";"                               { [push_def $1] }
| lstruct_decl ";"                        { [push_def $1] }
| ";"                                     { [] }

ldecl:
| lifetime_declaration decl_specs enter_scope_first enter_scope declarator leave_scope leave_scope_last { LDecl($1, make_decl $2 $5) }

function_def:
| decl_specs enter_scope_first declarator no_depth "{" list(item) "}" leave_scope_last    { FunctionDef(make_decl $1 $3, $8, SStmts(Depth(fst $4, snd $4), $6)) }
| decl_specs enter_scope_first declarator using_depth "{" list(item) "}" leave_scope_last    { FunctionDef(make_decl $1 $3, $8, SStmts(Depth(fst $4, snd $4), $6)) }
| lifetime_declaration decl_specs enter_scope_first enter_scope declarator no_depth "{" list(item) "}" leave_scope leave_scope_last    { LFunctionDef($1, make_decl $2 $5, $11, SStmts(Depth(fst $6, snd $6), $8)) }
| lifetime_declaration decl_specs enter_scope_first enter_scope declarator using_depth "{" list(item) "}" leave_scope leave_scope_last    { LFunctionDef($1, make_decl $2 $5, $11, SStmts(Depth(fst $6, snd $6), $8)) }
