%{
  open Ast;;
%}
 
%token <string> CST
%token <string> STR
%token <string> IDENT
%token AND DEF FOR TRUE FALSE IN NOT OR RETURN NONE WHILE IF ELSE
%token EOF COLON
%token LP RP COMMA LB RB
%token PLUS MINUS TIMES DIV MOD
%token EQ EQQ
%token NEWLINE BEGIN END
%token LEQ GEQ LE GE NEQ

%left OR
%left AND
%nonassoc NOT
%nonassoc LE LEQ GE GEQ EQQ NEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc uminus

%start file

%type <Ast.program> file

%%

file:
| NEWLINE? ; d = global_stmt+ ; NEWLINE ? ; EOF { d }
| NEWLINE* ; EOF { [] }
;
  
global_stmt:
| DEF ; name = IDENT ; LP ; args = separated_list(COMMA,IDENT) ; RP ; COLON ; bod =  suite  
        { GFunDef(name, args, bod, $loc) }
| s = stmt  { Gstmt(s, $loc) }
;

suite: b = simple_stmt ; NEWLINE { b }
  | NEWLINE ; BEGIN ; s = stmt+ ; END { Sblock(s, $loc) }
;

left_value:
| s = IDENT { Var(s, $loc) }
| l = expr; LB ; e = expr ; RB { Tab(l,e,$loc) }
;

simple_stmt: 
  | RETURN ; e = expr { Sreturn(e, $loc) } 
  | l = left_value ; EQ ; e = expr { Sassign(l,e, $loc) } 
  | e = expr { Sval(e, $loc) } 
;

stmt:
| s = simple_stmt ; NEWLINE  { s }
| FOR ; s = IDENT ; IN ; e = expr ; COLON ; b = suite  {Sfor(s,e,b, $loc) }
| IF ; e = expr ; COLON ; b=suite {Sif(e, b, $loc)}
| IF ; e = expr ; COLON ; b1 = suite ; ELSE ; COLON ; b2 = suite ; {Sifelse(e, b1, b2, $loc)}
| WHILE ; e = expr ; COLON ; b = suite  {Swhile(e,b, $loc) }
;

expr:
| c = const                      { Const(c, $loc) }
| l = left_value                 { Val(l, $loc)}
| e1 = expr o = op e2 = expr     { Op(o,e1,e2, $loc) }
| MINUS e = expr %prec uminus    { Moins(e, $loc) } 
| NOT e = expr                   { Not(e, $loc) } 
| s = IDENT ; LP ; args = separated_list(COMMA,expr) ; RP { Ecall(s,args, $loc) }
| LB ; args = separated_list(COMMA,expr) ; RB { List(args, $loc)}
| LB e = expr FOR var = IDENT IN l = expr RB { ComprehensionSimple (e, var, l, $loc) }
| LB e = expr FOR var = IDENT IN l = expr IF cond = expr RB { ComprehensionIf (e, var, l, cond, $loc) }
| LP ; e = expr ; RP { e }
;

%inline op:
| PLUS  { Add}
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
| MOD   { Mod }
| LEQ   { Leq }
| GEQ   { Geq }
| GE    { Ge  }
| LE    { Le  }
| NEQ   { Neq }
| EQQ   { Eq  }
| AND   { And }
| OR    { Or  } 
;

const:
| i = CST { Int(i, $loc) }
| s = STR { Str(s, $loc) }
| TRUE    { Bool(true, $loc)}
| FALSE   { Bool(false, $loc)}
| NONE    { Non($loc) }
;
