open Asm
open Util
open Lisp_expression

module Encode = struct
  let num_shift = 2

  let num_mask = 0b11

  let num_tag = 0b00

  let bool_shift = 7

  let bool_mask = 0b1111111

  let bool_tag = 0b0011111

  let encode (value : int) (shift : int) (tag : int) : int =
    (value lsl shift) lor tag

  let from_int (n : int) : int = encode n num_shift num_tag

  let from_bool (b : bool) : int =
    let bit = if b then 1 else 0 in
    encode bit bool_shift bool_tag
end

let rax : register = create_register Rax false

let rsp : register = create_register Rsp false

let r8 : register = create_register R8 false

let zf_to_bool : directive list =
  [ Mov (RegImm (rax, 0))
  ; Setz rax
  ; Shl (RegImm (rax, Encode.bool_shift))
  ; Or (RegImm (rax, Encode.bool_tag)) ]

let lf_to_bool : directive list =
  [ Mov (RegImm (rax, 0))
  ; Setl rax
  ; Shl (RegImm (rax, Encode.bool_shift))
  ; Or (RegImm (rax, Encode.bool_tag)) ]

type compile_body_result = Error | Correct of directive list

let rec compile_body (symbol_table : int Symtab.t) (stack_index : int)
    (expression : lisp_expression) : compile_body_result =
  match expression with
  | Number n ->
      Correct [Mov (RegImm (rax, Encode.from_int n))]
  | Boolean b ->
      Correct [Mov (RegImm (rax, Encode.from_bool b))]
  | Not arg -> (
    match compile_body symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Cmp (RegImm (rax, Encode.from_bool false))]
          @ zf_to_bool ) )
  | Is_zero arg -> (
    match compile_body symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [Cmp (RegImm (rax, Encode.from_int 0))]
          @ zf_to_bool ) )
  | Is_num arg -> (
    match compile_body symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          ( arg_directives
          @ [ And (RegImm (rax, Encode.num_mask))
            ; Cmp (RegImm (rax, Encode.num_tag)) ]
          @ zf_to_bool ) )
  | Add1 arg -> (
    match compile_body symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          (arg_directives @ [Add (RegImm (rax, Encode.from_int 1))]) )
  | Sub1 arg -> (
    match compile_body symbol_table stack_index arg with
    | Error ->
        Error
    | Correct arg_directives ->
        Correct
          (arg_directives @ [Sub (RegImm (rax, Encode.from_int 1))]) )
  | If {conditional; consequent; alternative} -> (
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      match compile_body symbol_table stack_index conditional with
      | Error ->
          Error
      | Correct conditional_directives -> (
        match compile_body symbol_table stack_index consequent with
        | Error ->
            Error
        | Correct consequent_directives -> (
          match compile_body symbol_table stack_index alternative with
          | Error ->
              Error
          | Correct alternative_directives ->
              Correct
                ( conditional_directives
                @ [ Cmp (RegImm (rax, Encode.from_bool false))
                  ; Jz else_label ]
                @ consequent_directives @ [Jmp continue_label]
                @ [Label else_label] @ alternative_directives
                @ [Label continue_label] ) ) ) )
  | Add (arg1, arg2) -> (
    match compile_body symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match compile_body symbol_table (stack_index - 8) arg2 with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives
            @ [Mov (MemReg (arg1_memory_address, rax))]
            @ arg2_directives
            @ [Mov (RegMem (r8, arg1_memory_address))]
            @ [Add (RegReg (rax, r8))] ) ) )
  | Sub (arg1, arg2) -> (
    match compile_body symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match compile_body symbol_table (stack_index - 8) arg2 with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives
            @ [Mov (MemReg (arg1_memory_address, rax))]
            @ arg2_directives
            @ [Mov (RegMem (r8, arg1_memory_address))]
            @ [Sub (RegReg (rax, r8))] ) ) )
  | Eq (arg1, arg2) -> (
    match compile_body symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match compile_body symbol_table (stack_index - 8) arg2 with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives
            @ [Mov (MemReg (arg1_memory_address, rax))]
            @ arg2_directives
            @ [Mov (RegMem (r8, arg1_memory_address))]
            @ [Cmp (RegReg (rax, r8))]
            @ zf_to_bool ) ) )
  | Lt (arg1, arg2) -> (
    match compile_body symbol_table stack_index arg1 with
    | Error ->
        Error
    | Correct arg1_directives -> (
      match compile_body symbol_table (stack_index - 8) arg2 with
      | Error ->
          Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct
            ( arg1_directives
            @ [Mov (MemReg (arg1_memory_address, rax))]
            @ arg2_directives
            @ [Mov (RegMem (r8, arg1_memory_address))]
            @ [Cmp (RegReg (r8, rax))]
            @ lf_to_bool ) ) )
  | Let {name; value; body} -> (
    match compile_body symbol_table stack_index value with
    | Error ->
        Error
    | Correct value_directives -> (
      match
        compile_body
          (Symtab.add name stack_index symbol_table)
          (stack_index - 8) body
      with
      | Error ->
          Error
      | Correct body_directives ->
          Correct
            ( value_directives
            @ [Mov (MemReg (stack_address stack_index, rax))]
            @ body_directives ) ) )
  | Var name -> (
    match Symtab.find_opt name symbol_table with
    | None ->
        Error
    | Some location ->
        Correct [Mov (RegMem (rax, stack_address location))] )

let compile (expression : lisp_expression) : directive list =
  let symbol_table = Symtab.empty in
  match compile_body symbol_table (-8) expression with
  | Error ->
      failwith ""
  | Correct body ->
      [Global "entry"; Label "entry"] @ body

let run (expression : lisp_expression) : string =
  let assembly =
    expression |> compile
    |> List.map directive_to_string
    |> String.concat "\n"
  in
  let file = open_out "program.s" in
  output_string file assembly ;
  close_out file ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp ; r
