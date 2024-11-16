open Parser
open Lexing
open Lexer
open Printf

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "À la ligne %d, autour des positions %d-%d:\n" l (c - 1) c

let eval_file ifile =
  let f = open_in ifile in
  let buf = Lexing.from_channel f in
  let _ =
    try
      let program_ast = file take_buffered buf in
      Runtime.eval program_ast
    with
    | Lexer.Lexing_error c ->
        (* Erreur lexicale. On récupère sa position absolue et
           on la convertit en numéro de ligne *)
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur dans l'analyse lexicale: %c.\n" c;
        exit 1
    | Parser.Error ->
        (* Erreur syntaxique. On récupère sa position absolue et on la
           convertit en numéro de ligne *)
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Erreur dans l'analyse syntaxique.\n";
        exit 2
    | Runtime.RuntimeError (msg, pos) ->
        eprintf "Erreur dynamique ligne %d : %s\n" pos.pos_lnum msg;
        exit 3
    | _ ->
        localisation (Lexing.lexeme_start_p buf);
        eprintf "Une erreur inconnue est intervenue.\n";
        exit 4
  in
  close_in f
