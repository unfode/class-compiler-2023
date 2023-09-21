open S_exp
open Asm
open Util

exception BadExpression of s_exp

let zf_to_bool : directive list =
  [ Mov (Reg Rax, Imm (Int 0))
  ; Setz (Reg Rax)
  ; Shl (Reg Rax, Imm (Int bool_shift))
  ; Or (Reg Rax, Imm (Int bool_tag)) ]

let rec compile_exp (exp : s_exp) : directive list =
  match exp with
  | Sym "true" ->
      [Mov (Reg Rax, Imm (Bool true))]
  | Sym "false" ->
      [Mov (Reg Rax, Imm (Bool false))]
  | Num n ->
      [Mov (Reg Rax, Imm (Int n))]
  | Lst [Sym "add1"; arg] ->
      compile_exp arg @ [Add (Reg Rax, Imm (Int 1))]
  | Lst [Sym "sub1"; arg] ->
      compile_exp arg @ [Sub (Reg Rax, Imm (Int 1))]
  | Lst [Sym "not"; arg] ->
      compile_exp arg @ [Cmp (Reg Rax, Imm (Bool false))] @ zf_to_bool
  | Lst [Sym "zero?"; arg] ->
      compile_exp arg @ [Cmp (Reg Rax, Imm (Int 0))] @ zf_to_bool
  | Lst [Sym "num?"; arg] ->
      compile_exp arg
      @ [ And (Reg Rax, Imm (Int num_mask))
        ; Cmp (Reg Rax, Imm (Int num_tag)) ]
      @ zf_to_bool
  | Lst [Sym "if"; test_exp; then_exp; else_exp] ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp test_exp
      @ [Cmp (Reg Rax, Imm (Bool false)); Jz else_label]
      @ compile_exp then_exp @ [Jmp continue_label]
      @ [Label else_label] @ compile_exp else_exp
      @ [Label continue_label]
  | e ->
      raise (BadExpression e)

let compile (program : s_exp) : string =
  [Global "entry"; Label "entry"] @ compile_exp program @ [Ret]
  |> List.map string_of_directive
  |> String.concat "\n"

let compile_to_file (program : string) : unit =
  let file = open_out "program.s" in
  output_string file (compile (parse program)) ;
  close_out file

let compile_and_run (program : string) : string =
  compile_to_file program ;
  ignore (Unix.system "nasm program.s -f elf64 -o program.o") ;
  ignore
    (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
  let inp = Unix.open_process_in "./program" in
  let r = input_line inp in
  close_in inp ; r
