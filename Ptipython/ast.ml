type ppos = Lexing.position * Lexing.position

type program = global_stmt list

and global_stmt =
  | GFunDef of string * string list * stmt * ppos
  | Gstmt of stmt * ppos

and stmt =
  | Sfor of string * expr * stmt * ppos
  | Sblock of stmt list * ppos
  | Sreturn of expr * ppos
  | Sassign of left_value * expr * ppos
  | Sval of expr * ppos
  | Sif of expr * stmt * ppos
  | Sifelse of expr * stmt * stmt * ppos
  | Swhile of expr * stmt * ppos

and const =
  | Int of string * ppos
  | Str of string * ppos
  | Bool of bool * ppos
  | Non of ppos

and left_value = Tab of expr * expr * ppos | Var of string * ppos

and expr =
  | Const of const * ppos
  | Val of left_value * ppos
  | Moins of expr * ppos
  | Not of expr * ppos
  | Op of binop * expr * expr * ppos
  | List of expr list * ppos
  | Ecall of string * expr list * ppos
  | ComprehensionSimple of expr * string * expr * ppos
  | ComprehensionIf of expr * string * expr * expr * ppos

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Leq
  | Le
  | Geq
  | Ge
  | Neq
  | Eq
  | And
  | Or

let str_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Leq -> "<="
  | Le -> "<"
  | Geq -> ">="
  | Ge -> ">"
  | Neq -> "!="
  | Eq -> "=="
  | And -> "&&"
  | Or -> "||"
