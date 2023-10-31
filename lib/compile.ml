open Asm
open Util
open Lisp_expression
open Encode

let zf_to_bool : directive list =
  [ Mov (RegImm (Rax, 0))
  ; Setz Rax
  ; Shl (RegImm (Rax, bool_spec.shift))
  ; Or (RegImm (Rax, bool_spec.tag)) ]

let lf_to_bool : directive list =
  [ Mov (RegImm (Rax, 0))
  ; Setl Rax
  ; Shl (RegImm (Rax, bool_spec.shift))
  ; Or (RegImm (Rax, bool_spec.tag)) ]

let error_function_name : string = "error"

let read_num_function_name : string = "read_num"

let print_function_name : string = "print_value"

let print_newline_function_name : string = "print_newline"

let assert_type (target : register) (temporary : register)
    (spec : datatype_spec) : directive list =
  [ Mov (RegReg (temporary, target))
  ; And (RegImm (temporary, get_mask spec))
  ; Cmp (RegImm (temporary, spec.tag))
  ; Jnz error_function_name ]

let assert_is_number (target : register) (temporary : register) :
    directive list =
  assert_type target temporary num_spec

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

type compile_result = Error | Correct of directive list

let rec compile_body (definitions : definition Symtab.t)
    (symbol_table : int Symtab.t) (stack_index : int)
    (expression : lisp_expression) : compile_result =
  match expression with
  | Number n ->
      Correct [Mov (RegImm (Rax, encode_int n))]
  | Boolean b ->
      Correct [Mov (RegImm (Rax, encode_bool b))]
  | Not arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Cmp (RegImm (Rax, encode_bool false))]
          @ zf_to_bool ) )
  | Is_zero arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Cmp (RegImm (Rax, encode_int 0))]
          @ zf_to_bool ) )
  | Is_num arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [ And (RegImm (Rax, get_mask num_spec))
            ; Cmp (RegImm (Rax, num_spec.tag)) ]
          @ zf_to_bool ) )
  | Add1 arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives @ assert_is_number Rax R8
          @ [Add (RegImm (Rax, encode_int 1))] ) )
  | Sub1 arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives @ assert_is_number Rax R8
          @ [Sub (RegImm (Rax, encode_int 1))] ) )
  | If {conditional; consequent; alternative} -> (
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      match
        compile_body definitions symbol_table stack_index conditional
      with
      | Error ->
          Error
      | Correct conditional_directives -> (
        match
          compile_body definitions symbol_table stack_index consequent
        with
        | Error ->
            Error
        | Correct consequent_directives -> (
          match
            compile_body definitions symbol_table stack_index
              alternative
          with
          | Error ->
              Error
          | Correct alternative_directives ->
              Correct
                ( conditional_directives
                @ [ Cmp (RegImm (Rax, encode_bool false))
                  ; Jz else_label ]
                @ consequent_directives @ [Jmp continue_label]
                @ [Label else_label] @ alternative_directives
                @ [Label continue_label] ) ) ) )
  | Add (arg1, arg2) -> (
    match compile_body definitions symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match
        compile_body definitions symbol_table (stack_index - 8) arg2
      with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives @ assert_is_number Rax R8
            @ [Mov (MemReg (arg1_memory_address, Rax))]
            @ arg2_directives @ assert_is_number Rax R8
            @ [Mov (RegMem (R8, arg1_memory_address))]
            @ [Add (RegReg (Rax, R8))] ) ) )
  | Sub (arg1, arg2) -> (
    match compile_body definitions symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match
        compile_body definitions symbol_table (stack_index - 8) arg2
      with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives @ assert_is_number Rax R8
            @ [Mov (MemReg (arg1_memory_address, Rax))]
            @ arg2_directives @ assert_is_number Rax R8
            @ [Mov (RegMem (R8, arg1_memory_address))]
            @ [Sub (RegReg (Rax, R8))] ) ) )
  | Eq (arg1, arg2) -> (
    match compile_body definitions symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match
        compile_body definitions symbol_table (stack_index - 8) arg2
      with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives
            @ [Mov (MemReg (arg1_memory_address, Rax))]
            @ arg2_directives
            @ [Mov (RegMem (R8, arg1_memory_address))]
            @ [Cmp (RegReg (Rax, R8))]
            @ zf_to_bool ) ) )
  | Lt (arg1, arg2) -> (
    match compile_body definitions symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match
        compile_body definitions symbol_table (stack_index - 8) arg2
      with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives @ assert_is_number Rax R8
            @ [Mov (MemReg (arg1_memory_address, Rax))]
            @ arg2_directives @ assert_is_number Rax R8
            @ [Mov (RegMem (R8, arg1_memory_address))]
            @ [Cmp (RegReg (R8, Rax))]
            @ lf_to_bool ) ) )
  | Let {name; value; body} -> (
    match compile_body definitions symbol_table stack_index value with
    | Error ->
        Error
    | Correct value_directives -> (
      match
        compile_body definitions
          (Symtab.add name stack_index symbol_table)
          (stack_index - 8) body
      with
      | Error ->
          Error
      | Correct body_directives ->
          Correct
            ( value_directives
            @ [Mov (MemReg (stack_address stack_index, Rax))]
            @ body_directives ) ) )
  | Var name -> (
    match Symtab.find_opt name symbol_table with
    | None ->
        Error
    | Some location ->
        Correct [Mov (RegMem (Rax, stack_address location))] )
  | Pair (left, right) -> (
    match compile_body definitions symbol_table stack_index left with
    | Error ->
        Error
    | Correct left_directives -> (
      match
        compile_body definitions symbol_table (stack_index - 8) right
      with
      | Error ->
          Error
      | Correct right_directives ->
          let left_memory_address = stack_address stack_index in
          Correct
            ( left_directives
            @ [Mov (MemReg (left_memory_address, Rax))]
            @ right_directives
            @ [ Mov (RegMem (R8, left_memory_address))
              ; Mov (MemReg (Reg Rdi, R8))
              ; Mov (MemReg (RegImm (Rdi, 8), Rax))
              ; Mov (RegReg (Rax, Rdi))
              ; Or (RegImm (Rax, pair_spec.tag))
              ; Add (RegImm (Rdi, 16)) ] ) ) )
  | Left arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Mov (RegMem (Rax, RegImm (Rax, -pair_spec.tag)))] ) )
  | Right arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Mov (RegMem (Rax, RegImm (Rax, -pair_spec.tag + 8)))] ) )
  | Read_num ->
      Correct
        [ Mov (MemReg (stack_address stack_index, Rdi))
        ; Add (RegImm (Rsp, align_stack_index stack_index))
        ; Call read_num_function_name
        ; Sub (RegImm (Rsp, align_stack_index stack_index))
        ; Mov (RegMem (Rdi, stack_address stack_index)) ]
  | New_line ->
      Correct
        [ Mov (MemReg (stack_address stack_index, Rdi))
        ; Add (RegImm (Rsp, align_stack_index stack_index))
        ; Call print_newline_function_name
        ; Sub (RegImm (Rsp, align_stack_index stack_index))
        ; Mov (RegMem (Rdi, stack_address stack_index))
        ; Mov (RegImm (Rax, encode_bool true)) ]
  | Print arg -> (
    match compile_body definitions symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [ Mov (MemReg (stack_address stack_index, Rdi))
            ; Mov (RegReg (Rdi, Rax))
            ; Add (RegImm (Rsp, align_stack_index stack_index))
            ; Call print_function_name
            ; Sub (RegImm (Rsp, align_stack_index stack_index))
            ; Mov (RegMem (Rdi, stack_address stack_index))
            ; Mov (RegImm (Rax, encode_bool true)) ] ) )
  | Do args ->
      List.fold_left
        (fun result arg ->
          match result with
          | Error ->
              Error
          | Correct directives -> (
            match
              compile_body definitions symbol_table stack_index arg
            with
            | Error ->
                Error
            | Correct arg_directives ->
                Correct (directives @ arg_directives) ) )
        (Correct [Mov (RegImm (Rax, encode_bool true))])
        args
  | Call {function_name; arguments} -> (
    match Symtab.find_opt function_name definitions with
    | None ->
        Error
    | Some definition -> (
        if List.length definition.args <> List.length arguments then
          Error
        else
          let stack_base = align_stack_index (stack_index + 8) in
          let _, compiled_args =
            List.fold_left
              (fun (i, result) argument ->
                match result with
                | Error ->
                    (i, Error)
                | Correct directives -> (
                  match
                    compile_body definitions symbol_table
                      (stack_base - (8 * (i + 2)))
                      argument
                  with
                  | Error ->
                      (i, Error)
                  | Correct new_directives ->
                      ( i + 1
                      , Correct
                          ( directives @ new_directives
                          @ [ Mov
                                (MemReg
                                   ( stack_address
                                       (stack_base - (8 * (i + 2)))
                                   , Rax ) ) ] ) ) ) )
              (0, Correct []) arguments
          in
          match compiled_args with
          | Error ->
              Error
          | Correct arg_directives ->
              Correct
                ( arg_directives
                @ [ Add (RegImm (Rsp, stack_base))
                  ; Call function_name
                  ; Sub (RegImm (Rsp, stack_base)) ] ) ) )

let compile_definitions (definitions : definition Symtab.t) :
    compile_result =
  Symtab.fold
    (fun name definition result ->
      match result with
      | Error ->
          Error
      | Correct directives -> (
          let arg_addresses =
            List.mapi
              (fun i arg -> (arg, -8 * (i + 1)))
              definition.args
          in
          let function_call_env =
            List.fold_left
              (fun result (name, value) ->
                Symtab.add name value result )
              Symtab.empty arg_addresses
          in
          match
            compile_body definitions function_call_env
              (-8 * (List.length definition.args + 1))
              definition.body
          with
          | Error ->
              Error
          | Correct new_directives ->
              Correct
                (directives @ [Label name] @ new_directives @ [Ret]) )
      )
    definitions (Correct [])

let compile (program : program) : compile_result =
  match compile_definitions program.definitions with
  | Error ->
      Error
  | Correct definitions -> (
    match
      compile_body program.definitions Symtab.empty (-8) program.body
    with
    | Error ->
        Error
    | Correct body ->
        Correct
          ( [ Global "entry"
            ; Extern error_function_name
            ; Extern read_num_function_name
            ; Extern print_newline_function_name
            ; Extern print_function_name
            ; Label "entry" ]
          @ body @ [Ret] @ definitions ) )

let run (program : program) : string =
  match compile program with
  | Error ->
      failwith ""
  | Correct directives ->
      let assembly =
        directives
        |> List.map directive_to_string
        |> String.concat "\n"
      in
      let file = open_out "program.s" in
      output_string file assembly ;
      close_out file ;
      ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
      ignore
        (Unix.system
           "gcc program.o runtime.o -o program -z noexecstack" ) ;
      let inp = Unix.open_process_in "./program" in
      let r = input_line inp in
      close_in inp ; r
