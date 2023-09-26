open Lisp_expression

type value = Number of int | Boolean of bool

let value_to_string (v : value) : string =
  match v with
  | Number n ->
      string_of_int n
  | Boolean b ->
      if b then "true" else "false"

type interpret_result = Correct of value | Error

let interpret_result_to_string (result : interpret_result) : string =
  match result with
  | Error ->
      "error"
  | Correct value ->
      value_to_string value

let rec interpret (expression : lisp_expression) : interpret_result =
  match expression with
  | Number n ->
      Correct (Number n)
  | Boolean b ->
      Correct (Boolean b)
  | Not arg -> (
    match interpret arg with
    | Error ->
        Error
    | Correct arg_value ->
        if arg_value = Boolean false then Correct (Boolean true)
        else Correct (Boolean false) )
  | Is_zero arg -> (
    match interpret arg with
    | Error ->
        Error
    | Correct arg_value ->
        if arg_value = Number 0 then Correct (Boolean true)
        else Correct (Boolean false) )
  | Is_num arg -> (
    match interpret arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number _ ->
          Correct (Boolean true)
      | Boolean _ ->
          Correct (Boolean false) ) )
  | Add1 arg -> (
    match interpret arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number n ->
          Correct (Number (n + 1))
      | Boolean _ ->
          Error ) )
  | Sub1 arg -> (
    match interpret arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number n ->
          Correct (Number (n - 1))
      | Boolean _ ->
          Error ) )
  | If {conditional; consequent; alternative} -> (
    match interpret conditional with
    | Error ->
        Error
    | Correct conditional_value -> (
      match conditional_value with
      | Boolean false ->
          interpret alternative
      | Boolean true | Number _ ->
          interpret consequent ) )
