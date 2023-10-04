type register = Rax | Rcx | R8 | Rsp | Rdi

let register_to_string (reg : register) : string =
  match reg with
  | Rax ->
      "rax"
  | Rcx ->
      "rcx"
  | R8 ->
      "r8"
  | Rsp ->
      "rsp"
  | Rdi ->
      "rdi"

type memory_address =
  | Reg of register
  | Imm of int
  | RegImm of register * int

let memory_to_string (mem : memory_address) : string =
  let body =
    match mem with
    | Reg reg ->
        register_to_string reg
    | Imm i ->
        string_of_int i
    | RegImm (reg, i) ->
        register_to_string reg ^ " + " ^ string_of_int i
  in
  "[" ^ body ^ "]"

let stack_address (stack_index : int) : memory_address =
  RegImm (Rsp, stack_index)

type dest_src =
  | RegImm of register * int
  | RegReg of register * register
  | RegMem of register * memory_address
  | MemReg of memory_address * register
  | MemImm of memory_address * int

let dest_src_to_string (dest_src : dest_src) : string * string =
  match dest_src with
  | RegImm (reg, imm) ->
      (register_to_string reg, string_of_int imm)
  | RegReg (dest, src) ->
      (register_to_string dest, register_to_string src)
  | RegMem (reg, mem) ->
      (register_to_string reg, memory_to_string mem)
  | MemReg (mem, reg) ->
      (memory_to_string mem, register_to_string reg)
  | MemImm (mem, imm) ->
      (memory_to_string mem, string_of_int imm)

type directive =
  | Global of string
  | Extern of string
  | Label of string
  | Align of int
  | Mov of dest_src
  | Add of dest_src
  | Sub of dest_src
  | And of dest_src
  | Or of dest_src
  | Shl of dest_src
  | Shr of dest_src
  | Cmp of dest_src
  | Setz of register
  | Setl of register
  | Jmp of string
  | ComputedJmp of int
  | Jz of string
  | Jnz of string
  | Call of string
  | ComputedCall of int
  | Ret
  | Comment of string

let run cmd args =
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (run cmd args |- read_all)

let macos = run "uname" ["-s"] |> String.trim |> String.equal "Darwin"

let label_name macos name = if macos then "_" ^ name else name

let directive_to_string = function
  (* frontmatter *)
  | Global l ->
      Printf.sprintf
        (if macos then "default rel\nglobal %s" else "global %s")
        (label_name macos l)
  | Extern l ->
      Printf.sprintf "extern %s" (label_name macos l)
  (* labels *)
  | Label l ->
      label_name macos l ^ ":"
  | Align i ->
      Printf.sprintf "align %d" i
  (* actual instructions *)
  | Mov dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tmov %s, %s" dest_string src_string
  | Add dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tadd %s, %s" dest_string src_string
  | Sub dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tsub %s, %s" dest_string src_string
  | And dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tadd %s, %s" dest_string src_string
  | Or dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tor %s, %s" dest_string src_string
  | Shl dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tshl %s, %s" dest_string src_string
  | Shr dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tshr %s, %s" dest_string src_string
  | Cmp dest_src ->
      let dest_string, src_string = dest_src_to_string dest_src in
      Printf.sprintf "\tcmp %s, %s" dest_string src_string
  | Setz reg ->
      Printf.sprintf "\tsetz %s" (register_to_string reg)
  | Setl reg ->
      Printf.sprintf "\tsetl %s" (register_to_string reg)
  | Jmp name ->
      Printf.sprintf "\tjmp %s" (label_name macos name)
  | ComputedJmp _ ->
      "todo"
  | Jz name ->
      Printf.sprintf "\tjz %s" (label_name macos name)
  | Jnz name ->
      Printf.sprintf "\tjnz %s" (label_name macos name)
  | Call name ->
      Printf.sprintf "\tcall %s" (label_name macos name)
  | ComputedCall _ ->
      "todo"
  | Ret ->
      "\tret"
  | Comment s ->
      Printf.sprintf "; %s" s
