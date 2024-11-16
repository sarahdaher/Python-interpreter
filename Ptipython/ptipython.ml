open Printf

let _main =
  (* Vérification de la ligne de commande *)
  if Array.length Sys.argv <> 2 then (
    eprintf "usage: mypython file.py\n";
    exit 1);

  let ifile = Sys.argv.(1) in

  (* Vérification de l'extension .py *)
  if not (Filename.check_suffix ifile ".py") then (
    eprintf "Le fichier d'entrée doit avoir l'extension .py\n";
    exit 1);
  Eval.eval_file ifile
