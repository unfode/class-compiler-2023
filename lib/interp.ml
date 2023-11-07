open Lisp_expression
open Util

type value =
  | Number of int
  | Boolean of bool
  | Pair of value * value
  | Unit
  | Function of string

let rec value_to_string (v : value) : string =
  match v with
  | Number n -> string_of_int n
  | Boolean b -> if b then "true" else "false"
  | Pair (left, right) ->
    Printf.sprintf "(pair %s %s)" (value_to_string left)
      (value_to_string right)
  | Unit -> "()"
  | Function _ -> "<function>"

type interpret_result = Correct of value | Error

let interpret_result_to_string (result : interpret_result) : string =
  match result with
  | Error ->
      "error"
  | Correct value ->
      value_to_string value

let rec interpret_internal (definitions : definition Symtab.t)
    (env : value Symtab.t) (expression : lisp_expression) :
    interpret_result =
  match expression with
  | Number n ->
      Correct (Number n)
  | Boolean b ->
      Correct (Boolean b)
  | Not arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value ->
        if arg_value = Boolean false then Correct (Boolean true)
        else Correct (Boolean false) )
  | Is_zero arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value ->
        if arg_value = Number 0 then Correct (Boolean true)
        else Correct (Boolean false) )
  | Is_num arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number _ ->
          Correct (Boolean true)
      | Boolean _ | Pair _ | Unit | Function _ ->
          Correct (Boolean false) ) )
  | Add1 arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number n ->
          Correct (Number (n + 1))
      | Boolean _ | Pair _ | Unit | Function _ ->
          Error ) )
  | Sub1 arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number n ->
          Correct (Number (n - 1))
      | Boolean _ | Pair _ | Unit | Function _ ->
          Error ) )
  | If {conditional; consequent; alternative} -> (
    match interpret_internal definitions env conditional with
    | Error ->
        Error
    | Correct conditional_value -> (
      match conditional_value with
      | Boolean false ->
          interpret_internal definitions env alternative
      | Boolean true | Number _ | Pair _ | Unit | Function _ ->
          interpret_internal definitions env consequent ) )
  | Add (operand1, operand2) -> (
    match interpret_internal definitions env operand1 with
    | Error ->
        Error
    | Correct operand1_value -> (
      match operand1_value with
      | Boolean _ | Pair _ | Unit | Function _ ->
          Error
      | Number n1 -> (
        match interpret_internal definitions env operand2 with
        | Error ->
            Error
        | Correct operand2_value -> (
          match operand2_value with
          | Boolean _ | Pair _ | Unit | Function _ ->
              Error
          | Number n2 ->
              Correct (Number (n1 + n2)) ) ) ) )
  | Sub (operand1, operand2) -> (
    match interpret_internal definitions env operand1 with
    | Error ->
        Error
    | Correct operand1_value -> (
      match operand1_value with
      | Boolean _ | Pair _ | Unit | Function _ ->
          Error
      | Number n1 -> (
        match interpret_internal definitions env operand2 with
        | Error ->
            Error
        | Correct operand2_value -> (
          match operand2_value with
          | Boolean _ | Pair _ | Unit | Function _ ->
              Error
          | Number n2 ->
              Correct (Number (n1 - n2)) ) ) ) )
  | Eq (operand1, operand2) -> (
    match interpret_internal definitions env operand1 with
    | Error ->
        Error
    | Correct operand1_value -> (
      match interpret_internal definitions env operand2 with
      | Error ->
          Error
      | Correct operand2_value ->
          Correct (Boolean (operand1_value = operand2_value)) ) )
  | Lt (operand1, operand2) -> (
    match interpret_internal definitions env operand1 with
    | Error ->
        Error
    | Correct operand1_value -> (
      match operand1_value with
      | Boolean _ | Pair _ | Unit | Function _ ->
          Error
      | Number n1 -> (
        match interpret_internal definitions env operand2 with
        | Error ->
            Error
        | Correct operand2_value -> (
          match operand2_value with
          | Boolean _ | Pair _ | Unit | Function _ ->
              Error
          | Number n2 ->
              Correct (Boolean (n1 < n2)) ) ) ) )
  | Var name -> (
    match Symtab.find_opt name definitions with
    | Some _ -> Correct (Function name)
    | None -> (
      match Symtab.find_opt name env with
      | None -> Error
      | Some value -> Correct value
    )
  )
  | Let {name; value; body} -> (
    match interpret_internal definitions env value with
    | Error ->
        Error
    | Correct value ->
        let new_env = Symtab.add name value env in
        interpret_internal definitions new_env body )
  | Pair (left, right) -> (
    match interpret_internal definitions env left with
    | Error ->
        Error
    | Correct left_value -> (
      match interpret_internal definitions env right with
      | Error ->
          Error
      | Correct right_value ->
          Correct (Pair (left_value, right_value)) ) )
  | Left arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number _ | Boolean _ | Unit | Function _ ->
          Error
      | Pair (left, _) ->
          Correct left ) )
  | Right arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value -> (
      match arg_value with
      | Number _ | Boolean _ | Unit | Function _ ->
          Error
      | Pair (_, right) ->
          Correct right ) )
  | Read_num -> (
    match input_line stdin |> int_of_string_opt with
    | None ->
        Error
    | Some n ->
        Correct (Number n) )
  | New_line ->
      output_string stdout "\n" ;
      Correct Unit
  | Print arg -> (
    match interpret_internal definitions env arg with
    | Error ->
        Error
    | Correct arg_value ->
        output_string stdout (value_to_string arg_value) ;
        Correct Unit )
  | Do {first; last} -> (
    let first_success = (
      List.fold_left
        (
          fun success expr -> (
            if not success then false
            else (
              match interpret_internal definitions env expr with
              | Error -> false
              | Correct _ -> true
            )
          )
        )
        true
        first
    ) in
    if not first_success then Error
    else interpret_internal definitions env last
  )
  | Call {function_; arguments} -> (
    match interpret_internal definitions env function_ with
    | Error -> Error
    | Correct function_value -> (
      match function_value with
      | Number _ | Boolean _ | Unit | Pair _ -> Error
      | Function function_name -> (
        match Symtab.find_opt function_name definitions with
        | None -> Error
        | Some definition -> (
            if List.length definition.args <> List.length arguments then
              Error
            else
              let values =
                List.fold_left
                  (
                    fun result argument -> (
                      match result with
                      | None -> None
                      | Some values -> (
                        match interpret_internal definitions env argument with
                        | Error -> None
                        | Correct value -> Some (values @ [value])
                      )
                    )
                  )
                  (Some [])
                  arguments
              in
              match values with
              | None -> Error
              | Some values -> (
                let function_call_env =
                  List.fold_left
                    (fun result (name, value) -> Symtab.add name value result)
                    Symtab.empty
                    (List.combine definition.args values)
                in
                interpret_internal definitions function_call_env definition.body
              )
        )
      )
    )
  )

let interpret (program : program) : interpret_result =
  interpret_internal program.definitions Symtab.empty program.body
