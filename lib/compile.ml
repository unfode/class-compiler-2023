open Asm
open Util
open Lisp_expression

let zf_to_bool : directive list =
  let rax = create_register Rax false in
  [ Mov (RegImm (rax, Literal 0))
  ; Setz rax
  ; Shl (RegImm (rax, Literal bool_shift))
  ; Or (RegImm (rax, Literal bool_tag)) ]

let rec compile (expression : lisp_expression) : directive list =
  let body =
    match expression with
    | Number n ->
        let rax = create_register Rax false in
        [Mov (RegImm (rax, Encoding (Int n)))]
    | Boolean b ->
        let rax = create_register Rax false in
        [Mov (RegImm (rax, Encoding (Bool b)))]
    | Not arg ->
        let rax = create_register Rax false in
        compile arg
        @ [Cmp (RegImm (rax, Encoding (Bool false)))]
        @ zf_to_bool
    | Is_zero arg ->
        let rax = create_register Rax false in
        compile arg
        @ [Cmp (RegImm (rax, Encoding (Int 0)))]
        @ zf_to_bool
    | Is_num arg ->
        let rax = create_register Rax false in
        compile arg
        @ [ And (RegImm (rax, Literal num_mask))
          ; Cmp (RegImm (rax, Literal num_tag)) ]
        @ zf_to_bool
    | Add1 arg ->
        let rax = create_register Rax false in
        compile arg @ [Add (RegImm (rax, Encoding (Int 1)))]
    | Sub1 arg ->
        let rax = create_register Rax false in
        compile arg @ [Sub (RegImm (rax, Encoding (Int 1)))]
    | If {conditional; consequent; alternative} ->
        let rax = create_register Rax false in
        let else_label = gensym "else" in
        let continue_label = gensym "continue" in
        compile conditional
        @ [Cmp (RegImm (rax, Encoding (Bool false))); Jz else_label]
        @ compile consequent @ [Jmp continue_label]
        @ [Label else_label] @ compile alternative
        @ [Label continue_label]
  in
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
