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

let rec compile_body (stack_index : int) (expression : lisp_expression)
    : directive list =
  match expression with
  | Number n ->
      [Mov (RegImm (rax, Encode.from_int n))]
  | Boolean b ->
      [Mov (RegImm (rax, Encode.from_bool b))]
  | Not arg ->
      compile_body stack_index arg
      @ [Cmp (RegImm (rax, Encode.from_bool false))]
      @ zf_to_bool
  | Is_zero arg ->
      compile_body stack_index arg
      @ [Cmp (RegImm (rax, Encode.from_int 0))]
      @ zf_to_bool
  | Is_num arg ->
      compile_body stack_index arg
      @ [ And (RegImm (rax, Encode.num_mask))
        ; Cmp (RegImm (rax, Encode.num_tag)) ]
      @ zf_to_bool
  | Add1 arg ->
      compile_body stack_index arg
      @ [Add (RegImm (rax, Encode.from_int 1))]
  | Sub1 arg ->
      compile_body stack_index arg
      @ [Sub (RegImm (rax, Encode.from_int 1))]
  | If {conditional; consequent; alternative} ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_body stack_index conditional
      @ [Cmp (RegImm (rax, Encode.from_bool false)); Jz else_label]
      @ compile_body stack_index consequent
      @ [Jmp continue_label] @ [Label else_label]
      @ compile_body stack_index alternative
      @ [Label continue_label]
  | Add (arg1, arg2) ->
      let arg1_directives = compile_body stack_index arg1 in
      let arg1_memory_location : memory = RegImm (rsp, stack_index) in
      let arg2_directives = compile_body (stack_index - 8) arg2 in
      arg1_directives
      @ [Mov (MemReg (arg1_memory_location, rax))]
      @ arg2_directives
      @ [Mov (RegMem (r8, arg1_memory_location))]
      @ [Add (RegReg (rax, r8))]
  | Sub (arg1, arg2) ->
      let arg1_directives = compile_body stack_index arg1 in
      let arg1_memory_location : memory = RegImm (rsp, stack_index) in
      let arg2_directives = compile_body (stack_index - 8) arg2 in
      arg1_directives
      @ [Mov (MemReg (arg1_memory_location, rax))]
      @ arg2_directives
      @ [Mov (RegMem (r8, arg1_memory_location))]
      @ [Sub (RegReg (rax, r8))]
  | Eq (arg1, arg2) ->
      let arg1_directives = compile_body stack_index arg1 in
      let arg1_memory_location : memory = RegImm (rsp, stack_index) in
      let arg2_directives = compile_body (stack_index - 8) arg2 in
      arg1_directives
      @ [Mov (MemReg (arg1_memory_location, rax))]
      @ arg2_directives
      @ [Mov (RegMem (r8, arg1_memory_location))]
      @ [Cmp (RegReg (rax, r8))]
      @ zf_to_bool
  | Lt (arg1, arg2) ->
      let arg1_directives = compile_body stack_index arg1 in
      let arg1_memory_location : memory = RegImm (rsp, stack_index) in
      let arg2_directives = compile_body (stack_index - 8) arg2 in
      arg1_directives
      @ [Mov (MemReg (arg1_memory_location, rax))]
      @ arg2_directives
      @ [Mov (RegMem (r8, arg1_memory_location))]
      @ [Cmp (RegReg (r8, rax))]
      @ lf_to_bool

let compile (expression : lisp_expression) : directive list =
  let body = compile_body (-8) expression in
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
