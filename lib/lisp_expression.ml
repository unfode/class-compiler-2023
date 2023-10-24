open S_exp

type lisp_expression =
  | Number of int
  | Boolean of bool
  | Not of lisp_expression
  | Is_zero of lisp_expression
  | Is_num of lisp_expression
  | Add1 of lisp_expression
  | Sub1 of lisp_expression
  | If of
      { conditional: lisp_expression
      ; consequent: lisp_expression
      ; alternative: lisp_expression }
  | Add of lisp_expression * lisp_expression
  | Sub of lisp_expression * lisp_expression
  | Eq of lisp_expression * lisp_expression
  | Lt of lisp_expression * lisp_expression
  | Var of string
  | Let of
      {name: string; value: lisp_expression; body: lisp_expression}
  | Pair of lisp_expression * lisp_expression
  | Left of lisp_expression
  | Right of lisp_expression
  | Read_num
  | Print of lisp_expression
  | New_line
  | Do of lisp_expression list

let rec s_exp_to_lisp_expression (s_expression : s_exp) :
    lisp_expression option =
  match s_expression with
  | Num n ->
      Some (Number n)
  | Sym "true" ->
      Some (Boolean true)
  | Sym "false" ->
      Some (Boolean false)
  | Sym var ->
      Some (Var var)
  | Lst [Sym "let"; Lst [Lst [Sym s; e]]; body] -> (
    match
      (s_exp_to_lisp_expression e, s_exp_to_lisp_expression body)
    with
    | Some value, Some body ->
        Some (Let {name= s; value; body})
    | _ ->
        None )
  | Lst [Sym "read-num"] ->
      Some Read_num
  | Lst [Sym "newline"] ->
      Some New_line
  | Lst [Sym "print"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_expr ->
        Some (Print arg_expr) )
  | Lst [Sym "not"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_lisp_expr ->
        Some (Not arg_lisp_expr) )
  | Lst [Sym "zero?"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_lisp_expr ->
        Some (Is_zero arg_lisp_expr) )
  | Lst [Sym "num?"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_lisp_expr ->
        Some (Is_num arg_lisp_expr) )
  | Lst [Sym "add1"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_lisp_expr ->
        Some (Add1 arg_lisp_expr) )
  | Lst [Sym "sub1"; arg] -> (
    match s_exp_to_lisp_expression arg with
    | None ->
        None
    | Some arg_lisp_expr ->
        Some (Sub1 arg_lisp_expr) )
  | Lst [Sym "if"; test_exp; then_exp; else_exp] -> (
    match s_exp_to_lisp_expression test_exp with
    | None ->
        None
    | Some test_lisp_expr -> (
      match s_exp_to_lisp_expression then_exp with
      | None ->
          None
      | Some then_lisp_expr -> (
        match s_exp_to_lisp_expression else_exp with
        | None ->
            None
        | Some else_lisp_expr ->
            Some
              (If
                 { conditional= test_lisp_expr
                 ; consequent= then_lisp_expr
                 ; alternative= else_lisp_expr } ) ) ) )
  | Lst [Sym "+"; arg1; arg2] -> (
    match s_exp_to_lisp_expression arg1 with
    | None ->
        None
    | Some arg1_lisp_expr -> (
      match s_exp_to_lisp_expression arg2 with
      | None ->
          None
      | Some arg2_lisp_expr ->
          Some (Add (arg1_lisp_expr, arg2_lisp_expr)) ) )
  | Lst [Sym "-"; arg1; arg2] -> (
    match s_exp_to_lisp_expression arg1 with
    | None ->
        None
    | Some arg1_lisp_expr -> (
      match s_exp_to_lisp_expression arg2 with
      | None ->
          None
      | Some arg2_lisp_expr ->
          Some (Sub (arg1_lisp_expr, arg2_lisp_expr)) ) )
  | Lst [Sym "="; arg1; arg2] -> (
    match s_exp_to_lisp_expression arg1 with
    | None ->
        None
    | Some arg1_lisp_expr -> (
      match s_exp_to_lisp_expression arg2 with
      | None ->
          None
      | Some arg2_lisp_expr ->
          Some (Eq (arg1_lisp_expr, arg2_lisp_expr)) ) )
  | Lst [Sym "<"; arg1; arg2] -> (
    match s_exp_to_lisp_expression arg1 with
    | None ->
        None
    | Some arg1_lisp_expr -> (
      match s_exp_to_lisp_expression arg2 with
      | None ->
          None
      | Some arg2_lisp_expr ->
          Some (Lt (arg1_lisp_expr, arg2_lisp_expr)) ) )
  | _ ->
      None
