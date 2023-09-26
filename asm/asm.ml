type register_kind = Rax | Rcx | R8 | Rsp | Rdi

type register = {kind: register_kind; last_byte: bool}

let create_register (kind : register_kind) (last_byte : bool) :
    register =
  {kind; last_byte}

let register_to_string (reg : register) : string =
  match reg.kind with
  | Rax ->
      if reg.last_byte then "al" else "rax"
  | Rcx ->
      if reg.last_byte then "cl" else "rcx"
  | R8 ->
      if reg.last_byte then "r8b" else "r8"
  | Rsp ->
      "rsp"
  | Rdi ->
      "rdi"

let num_shift = 2

let num_mask = 0b11

let num_tag = 0b00

let bool_shift = 7

let bool_mask = 0b1111111

let bool_tag = 0b0011111

type value_to_encode = Int of int | Bool of bool

let encode (value : value_to_encode) : int =
  match value with
  | Int i ->
      i lsl num_shift
  | Bool b ->
      let bit = if b then 1 else 0 in
      (bit lsl bool_shift) lor bool_tag

type immediate = Encoding of value_to_encode | Literal of int

let immediate_to_int (i : immediate) : int =
  match i with
  | Literal value ->
      value
  | Encoding value ->
      encode value

let immediate_to_string (i : immediate) : string =
  i |> immediate_to_int |> string_of_int

type dest_src =
  | RegImm of register * immediate
  | RegReg of register * register
  | RegMem of register
  | MemReg
  | MemImm

let dest_src_to_string (dest_src : dest_src) : string * string =
  match dest_src with
  | RegImm (reg, imm) ->
      (register_to_string reg, immediate_to_string imm)
  | RegReg (dest, src) ->
      (register_to_string dest, register_to_string src)
  | RegMem _ ->
      ("a", "b")
  | MemReg | MemImm ->
      ("c", "d")

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
  | Setl of int
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
  | Setz _ ->
      "todo"
  | Setl _ ->
      "todo"
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
