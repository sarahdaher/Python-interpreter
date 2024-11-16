open Ast

exception Error of string
exception RuntimeError of string * Lexing.position

type value =
  | VInt of int
  | VBool of bool
  | VStr of string
  | VNone
  | VList of value array
  | VRange of int * int * int

type func =
  | UserDefinedFunction of int * string list * stmt
  | BuiltinFunction of (value list -> value)

type env = {
  functions : (string, func) Hashtbl.t;
  global_vars : (string, value) Hashtbl.t;
  mutable local_vars : (string, value) Hashtbl.t list;
}

exception ReturnException of value

let set_variable env name value =
  match env.local_vars with
  | [] -> Hashtbl.add env.global_vars name value
  | local_vars :: _ -> Hashtbl.add local_vars name value

let rec string_of_value v escape =
  match v with
  | VInt i -> string_of_int i
  | VBool b -> if b then "True" else "False"
  | VStr s -> if escape then "'" ^ s ^ "'" else s
  | VNone -> "None"
  | VRange (min, max, step) -> Printf.sprintf "range(%d, %d, %d)" min max step
  | VList l ->
      let string_of_list l =
        let n = Array.length l in
        let s = ref "" in
        for i = 0 to n - 1 do
          s := !s ^ string_of_value l.(i) true;
          if i < n - 1 then s := !s ^ ", "
        done;
        !s
      in
      "[" ^ string_of_list l ^ "]"

let eval_const const =
  match const with
  | Int (v, _) -> VInt (int_of_string v)
  | Bool (b, _) -> VBool b
  | Str (s, _) -> VStr s
  | Non _ -> VNone

let range_to_array min max step =
  Array.init ((max - min + step - 1) / step) (fun i -> VInt (min + (i * step)))

let rec eval_op env op e1 e2 pos =
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  match op with
  | Add -> (
      match (v1, v2) with
      | VInt i1, VInt i2 -> VInt (i1 + i2)
      | VStr s1, VStr s2 -> VStr (s1 ^ s2)
      | VList l1, VList l2 ->
          let n1, n2 = (Array.length l1, Array.length l2) in
          let res = Array.make (n1 + n2) VNone in
          for i = 0 to n1 + n2 - 1 do
            if i < n1 then res.(i) <- l1.(i) else res.(i) <- l2.(i - n1)
          done;
          VList res
      | _ ->
          raise
            (RuntimeError
               ( "Operator + must be applied to two integers, two strings or \
                  two lists",
                 fst pos )))
  | Sub -> (
      match (v1, v2) with
      | VInt i1, VInt i2 -> VInt (i1 - i2)
      | _ ->
          raise
            (RuntimeError ("Operator + must be applied to two integers", fst pos))
      )
  | Mul -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VInt (v1 * v2)
      | VStr s1, VInt i1 | VInt i1, VStr s1 ->
          let rec duplicate_str res i str =
            if i <= 0 then res else duplicate_str (str ^ res) (i - 1) str
          in
          VStr (duplicate_str "" i1 s1)
      | VList l1, VInt i1 | VInt i1, VList l1 ->
          let rec duplicate_list res i lst =
            if i <= 0 then res
            else duplicate_list (Array.append lst res) (i - 1) lst
          in
          VList (duplicate_list [||] i1 l1)
      | _ ->
          raise
            (RuntimeError
               ( "Operator + must be applied to two integers, one integer and \
                  one string or one integer and one list",
                 fst pos )))
  | Div -> (
      match (v1, v2) with
      | VInt _, VInt 0 -> raise (RuntimeError ("Division by zero", fst pos))
      | VInt v1, VInt v2 -> VInt (v1 / v2)
      | _ ->
          raise
            (RuntimeError ("Operator / must be applied to two integers", fst pos))
      )
  | Mod -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VInt (v1 mod v2)
      | _ ->
          raise
            (RuntimeError ("Operator % must be applied to two integers", fst pos))
      )
  | And -> (
      match (v1, v2) with
      | VBool v1, VBool v2 -> VBool (v1 && v2)
      | _ ->
          raise
            (RuntimeError ("Operator / must be applied to two integers", fst pos))
      )
  | Or -> (
      match (v1, v2) with
      | VBool v1, VBool v2 -> VBool (v1 || v2)
      | _ ->
          raise
            (RuntimeError
               ("Operator or must be applied to two integers", fst pos)))
  | Eq -> VBool (v1 = v2)
  | Neq -> VBool (v1 <> v2)
  | Ge -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VBool (v1 > v2)
      | VStr v1, VStr v2 -> VBool (v1 > v2)
      | _ ->
          raise
            (RuntimeError
               ( "Operator > must be applied to two integers or two strings",
                 fst pos )))
  | Geq -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VBool (v1 >= v2)
      | VStr v1, VStr v2 -> VBool (v1 >= v2)
      | _ ->
          raise
            (RuntimeError
               ( "Operator >= must be applied to two integers or two strings",
                 fst pos )))
  | Le -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VBool (v1 < v2)
      | VStr v1, VStr v2 -> VBool (v1 < v2)
      | _ ->
          raise
            (RuntimeError
               ( "Operator < must be applied to two integers or two strings",
                 fst pos )))
  | Leq -> (
      match (v1, v2) with
      | VInt v1, VInt v2 -> VBool (v1 <= v2)
      | VStr v1, VStr v2 -> VBool (v1 <= v2)
      | _ ->
          raise
            (RuntimeError
               ( "Operator <= must be applied to two integers or two strings",
                 fst pos )))

and eval_call env name args pos =
  match Hashtbl.find_opt env.functions name with
  | None ->
      raise
        (RuntimeError (Printf.sprintf "Function %s not found" name, fst pos))
  | Some (BuiltinFunction f) ->
      let f_args = List.map (eval_expr env) args in
      f f_args
  | Some (UserDefinedFunction (nb_args, arg_names, stmt)) ->
      if nb_args = List.length args then (
        let return_value =
          try
            let table = Hashtbl.create 42 in
            let rec aux arg_names args =
              match (arg_names, args) with
              | name :: arg_names', arg :: args' ->
                  Hashtbl.add table name (eval_expr env arg);
                  aux arg_names' args'
              | _ -> ()
            in
            aux arg_names args;
            env.local_vars <- table :: env.local_vars;
            eval_stmt env stmt;
            VNone
          with ReturnException v -> v
        in
        env.local_vars <- List.tl env.local_vars;
        return_value)
      else
        raise
          (RuntimeError
             ( Printf.sprintf "Function %s take %d arguments" name nb_args,
               fst pos ))

and eval_val env left_value pos =
  let get_variable_opt name =
    match env.local_vars with
    | [] -> Hashtbl.find_opt env.global_vars name
    | local_vars :: _ -> (
        match Hashtbl.find_opt local_vars name with
        | Some v -> Some v
        | None -> Hashtbl.find_opt env.global_vars name)
  in
  let get_variable name =
    match get_variable_opt name with
    | Some v -> v
    | None ->
        raise
          (RuntimeError
             (Printf.sprintf "Variable %s does not exists" name, fst pos))
  in
  match left_value with
  | Var (name, _) -> get_variable name
  | Tab (tab_expr, indice_expr, pos) -> (
      let expr = eval_expr env tab_expr in
      let indice = eval_expr env indice_expr in
      match indice with
      | VInt indice -> (
          match expr with
          | VList l ->
              if indice < Array.length l then l.(indice)
              else raise (RuntimeError ("List index out of range", fst pos))
          | _ -> raise (RuntimeError ("Only lists can be indexed", fst pos)))
      | _ -> raise (RuntimeError ("Index must be an integer", fst pos)))

and comprehension_if env l var expr cond =
  let new_l = ref [] in
  for i = 0 to Array.length l - 1 do
    set_variable env var l.(i);
    if cond () then new_l := eval_expr env expr :: !new_l
  done;
  VList (Array.of_list (List.rev !new_l))

and non_false env expr =
  match eval_expr env expr with
  | VBool false | VInt 0 | VStr "" -> false
  | VList l when Array.length l = 0 -> false
  | _ -> true

and eval_expr env expr =
  match expr with
  | Const (const, _) -> eval_const const
  | List (l, _) ->
      let n = List.length l in
      let lst = Array.make n VNone in
      for i = 0 to n - 1 do
        lst.(i) <- eval_expr env (List.nth l i)
      done;
      VList lst
  | Moins (expr, pos) -> (
      match eval_expr env expr with
      | VInt v -> VInt (-v)
      | _ ->
          raise
            (RuntimeError ("Operator - must be applied to one iteger", fst pos))
      )
  | Op (op, e1, e2, pos) -> eval_op env op e1 e2 pos
  | Not (expr, pos) -> (
      match eval_expr env expr with
      | VBool v1 -> VBool (not v1)
      | _ ->
          raise
            (RuntimeError
               ("Operator not must be applied to one boolean", fst pos)))
  | Ecall (name, args, pos) -> eval_call env name args pos
  | Val (left_value, pos) -> eval_val env left_value pos
  | ComprehensionSimple (expr, var, l_expr, pos) -> (
      match eval_expr env l_expr with
      | VList l -> comprehension_if env l var expr (fun () -> true)
      | VRange (min, max, step) ->
          comprehension_if env (range_to_array min max step) var expr (fun () ->
              true)
      | _ ->
          raise
            (RuntimeError ("For must iterate on a list or on a range", fst pos))
      )
  | ComprehensionIf (expr, var, l_expr, cond_expr, pos) -> (
      match eval_expr env l_expr with
      | VList l ->
          comprehension_if env l var expr (fun () -> non_false env cond_expr)
      | VRange (min, max, step) ->
          comprehension_if env (range_to_array min max step) var expr (fun () ->
              non_false env cond_expr)
      | _ ->
          raise
            (RuntimeError ("For must iterate on a list or on a range", fst pos))
      )

and eval_for env name expr stmt pos =
  match eval_expr env expr with
  | VList l ->
      let n = Array.length l in
      for i = 0 to n - 1 do
        set_variable env name l.(i);
        eval_stmt env stmt
      done
  | VRange (min, max, step) ->
      let rec aux i =
        if i >= max then ()
        else (
          set_variable env name (VInt i);
          eval_stmt env stmt;
          aux (i + step))
      in
      aux min
  | _ ->
      raise (RuntimeError ("For must iterate on a list or on a range", fst pos))

and eval_assign env lv expr =
  let rec set_left_value left_value value =
    match left_value with
    | Var (name, _) -> (
        match env.local_vars with
        | [] -> Hashtbl.add env.global_vars name value
        | local_vars :: _ -> Hashtbl.add local_vars name value)
    | Tab (tab_expr, indice_expr, pos) -> (
        match eval_expr env indice_expr with
        | VInt indice -> (
            match tab_expr with
            | Val (lv, pos) -> (
                match eval_val env lv pos with
                | VList l ->
                    if indice < Array.length l && indice >= 0 then (
                      l.(indice) <- value;
                      set_left_value lv (VList l))
                    else
                      raise (RuntimeError ("List index out of range", fst pos))
                | _ ->
                    raise (RuntimeError ("Only lists can be indexed", fst pos)))
            | _ ->
                raise (RuntimeError ("Only variables can be modified", fst pos))
            )
        | _ -> raise (RuntimeError ("Index must be an integer", fst pos)))
  in
  set_left_value lv (eval_expr env expr)

and eval_stmt env stmt =
  match stmt with
  | Sreturn (expr, _) -> raise (ReturnException (eval_expr env expr))
  | Sval (expr, _) ->
      let _ = eval_expr env expr in
      ()
  | Sblock (l, _) ->
      List.iter
        (fun expr ->
          let _ = eval_stmt env expr in
          ())
        l
  | Sif (expr, stmt, _) -> if non_false env expr then eval_stmt env stmt else ()
  | Sifelse (expr, stmt_if, stmt_else, _) ->
      eval_stmt env (if non_false env expr then stmt_if else stmt_else)
  | Swhile (expr, stm, _) ->
      while non_false env expr do
        eval_stmt env stm
      done
  | Sfor (name, expr, stmt, pos) -> eval_for env name expr stmt pos
  | Sassign (lv, expr, _) -> eval_assign env lv expr

and string_of_left_value env left_value =
  match left_value with
  | Var (name, _) -> name
  | Tab (tab, indice_expr, pos) -> (
      let indice_value = eval_expr env indice_expr in
      match indice_value with
      | VInt indice -> (
          match tab with
          | Val (lv, _) ->
              string_of_left_value env lv ^ "[" ^ string_of_int indice ^ "]"
          | _ -> raise (RuntimeError ("A fatal error occured", fst pos)))
      | _ -> raise (RuntimeError ("A fatal error occured", fst pos)))

let declare_new_function functions name args stmt _ =
  let f = UserDefinedFunction (List.length args, args, stmt) in
  Hashtbl.replace functions name f

let eval_global_stmt env gstmt =
  match gstmt with
  | Gstmt (stmt, _) ->
      let _ = eval_stmt env stmt in
      ()
  | GFunDef (name, args, stmt, pos) ->
      declare_new_function env.functions name args stmt pos

let external_pos =
  {
    Lexing.pos_fname = "<stdlib>";
    Lexing.pos_cnum = 0;
    Lexing.pos_lnum = 0;
    Lexing.pos_bol = 0;
  }

let print_function =
  let f args =
    match args with
    | [ x ] ->
        print_endline (string_of_value x false);
        VNone
    | _ ->
        raise (RuntimeError ("Function print takes 1 argument", external_pos))
  in
  BuiltinFunction f

let type_function =
  let f args =
    match args with
    | [ x ] -> (
        match x with
        | VInt _ -> VStr "int"
        | VBool _ -> VStr "bool"
        | VStr _ -> VStr "str"
        | VNone -> VStr "NoneType"
        | VRange _ -> VStr "range"
        | VList _ -> VStr "list")
    | _ -> raise (RuntimeError ("Function type takes 1 argument", external_pos))
  in
  BuiltinFunction f

let len_function =
  let f args =
    match args with
    | [ VList l ] -> VInt (Array.length l)
    | _ ->
        raise
          (RuntimeError ("Function len takes 1 list in arguments", external_pos))
  in
  BuiltinFunction f

let range_function =
  let f args =
    match args with
    | [ VInt max ] -> VRange (0, max, 1)
    | [ VInt min; VInt max ] -> VRange (min, max, 1)
    | [ VInt min; VInt max; VInt step ] -> VRange (min, max, step)
    | _ ->
        raise
          (RuntimeError
             ( "Range function takes from 1 to 3 integers in arguments",
               external_pos ))
  in
  BuiltinFunction f

let eval program_ast =
  let functions = Hashtbl.create 42 in
  Hashtbl.add functions "print" print_function;
  Hashtbl.add functions "len" len_function;
  Hashtbl.add functions "type" type_function;
  Hashtbl.add functions "range" range_function;
  let global_vars = Hashtbl.create 42 in
  let env = { functions; global_vars; local_vars = [] } in
  let rec aux prog =
    match prog with
    | [] -> ()
    | gstmt :: rest ->
        eval_global_stmt env gstmt;
        aux rest
  in
  aux program_ast
