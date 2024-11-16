{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let kwd_tbl = ["and",AND; "def",DEF; "for",FOR; "True",TRUE;"False",FALSE;
                 "in",IN;"not",NOT; "or",OR ;"return",RETURN; "None",NONE; "while",WHILE; "if",IF; "else",ELSE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let pile = ref [0]

  let rec nouv n = match !pile with
    | a::q when a < n -> pile:=n::a::q;[BEGIN]
    | a::q when a > n -> pile:=q;END::(nouv n)
    | _a::_q -> []
    | [] -> failwith "1"

  let desescape s =
    let rec foo i =
      if String.length s = i
      then []
      else
        if i+1 < String.length s && s.[i]='\\'
        then
          let c = match s.[i+1] with
                    | 'n' -> '\n'
                    | 'r' -> '\r'
                    | 't' -> '\t'
                    | '"' -> '"'
                    | c -> raise (Lexing_error(c)) in
          c::foo (i+2)
        else s.[i]::foo(i+1)
     in            
     foo 0 |> List.map (String.make 1)|> String.concat ""
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = ['0'-'9']+
let space = [' ' '\t']
let chaine = ([^'\"']|'\\''\n'|'\\''\"')*

rule first = parse
  | space* '\n' { newline lexbuf ; first lexbuf }
  | space* as esp { NEWLINE::nouv (String.length esp) }
and token = parse
  | '\n' { newline lexbuf ; first lexbuf }
  | space+ { token lexbuf }
  | ident as id { [id_or_kwd id] }
  | '#'[^'\n']* { token lexbuf}
  | ':'     { [COLON] }
  | ','     { [COMMA] }
  | '+'     { [PLUS] }
  | '!''='  { [NEQ] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | '/''/'     { [DIV] }
  | '%'     { [MOD] }
  | '=''='  { [EQQ] }
  | '='     { [EQ] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | '['     { [LB] }
  | ']'     { [RB] }
  | '<''='  { [LEQ] }
  | '>''='  { [GEQ] }
  | '<'  { [LE] }
  | '>'  { [GE] }
  | integer as s { [CST (s)] }
  | eof     { [NEWLINE;EOF] }
  | '\"' (chaine as s) '\"' { [STR(desescape s)] }  
  | _ as c  { raise (Lexing_error c) }


{
  let rec take_buffered = 
    let buffer = ref None in
    fun lexbuf ->
      match !buffer with
      | Some (x::t) -> buffer := Some t ; x
      | Some [] ->  buffer := Some (token lexbuf) ; take_buffered lexbuf
      | None -> buffer := Some (first lexbuf) ; take_buffered lexbuf
}
