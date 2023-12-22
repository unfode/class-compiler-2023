open Asm
open Util
open Lisp_expression
open Encode

let zf_to_bool : directive list = [
  Mov (RegImm (Rax, 0));
  Setz Rax;
  Shl (RegImm (Rax, bool_spec.shift));
  Or (RegImm (Rax, bool_spec.tag))
]

let lf_to_bool : directive list = [
  Mov (RegImm (Rax, 0));
  Setl Rax;
  Shl (RegImm (Rax, bool_spec.shift));
  Or (RegImm (Rax, bool_spec.tag))
]

let error_function_name : string = "error"
let read_num_function_name : string = "read_num"
let print_function_name : string = "print_value"
let print_newline_function_name : string = "print_newline"

let assert_type (target : register) (temporary : register) (mask: int) (tag: int) : directive list = [
  Mov (RegReg (temporary, target));
  And (RegImm (temporary, mask));
  Cmp (RegImm (temporary, tag));
  Jnz error_function_name
]

let assert_value_type (target : register) (temporary : register) (spec : value_type_spec) : directive list =
  assert_type target temporary (get_mask ~shift:spec.shift) spec.tag

let assert_is_number (target : register) (temporary : register) : directive list =
  assert_value_type target temporary num_spec

let assert_reference_type (target : register) (temporary : register) (spec : reference_type_spec) : directive list =
  assert_type target temporary (get_mask ~shift:reference_type_shift) spec.tag

let assert_is_function (target : register) (temporary : register) : directive list =
  assert_reference_type target temporary function_spec

let align_stack_index (stack_index : int) : int =
  if stack_index mod 16 = -8 then stack_index else stack_index - 8

type compile_result = Error | Correct of directive list

let compile_number (n: int) : directive list = [Mov (RegImm (Rax, encode_int n))]
let compile_boolean (b: bool) : directive list = [Mov (RegImm (Rax, encode_bool b))]

let compile_var (symbol_table : int Symtab.t) (name: string) : compile_result = (
  match Symtab.find_opt name symbol_table with
  | None -> Error
  | Some location -> Correct [Mov (RegMem (Rax, stack_address location))]
)

let compile_new_line (stack_index: int) : directive list = [
  Mov (MemReg (stack_address stack_index, Rdi));
  Add (RegImm (Rsp, align_stack_index stack_index));
  Call print_newline_function_name;
  Sub (RegImm (Rsp, align_stack_index stack_index));
  Mov (RegMem (Rdi, stack_address stack_index));
  Mov (RegImm (Rax, encode_bool true))
]

let rec compile_non_tail (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (expression : lisp_expression) : compile_result = (
  match expression with
  | Number n -> Correct (compile_number n)
  | Boolean b -> Correct (compile_boolean b)
  | Not arg -> compile_not definitions symbol_table stack_index arg
  | Is_zero arg -> compile_is_zero definitions symbol_table stack_index arg
  | Is_num arg -> compile_is_num definitions symbol_table stack_index arg
  | Add1 arg -> compile_add1 definitions symbol_table stack_index arg
  | Sub1 arg -> compile_sub1 definitions symbol_table stack_index arg
  | Add (arg1, arg2) -> compile_add definitions symbol_table stack_index arg1 arg2
  | Sub (arg1, arg2) -> compile_sub definitions symbol_table stack_index arg1 arg2
  | Eq (arg1, arg2) -> compile_eq definitions symbol_table stack_index arg1 arg2
  | Lt (arg1, arg2) -> compile_lt definitions symbol_table stack_index arg1 arg2
  | Let {name; value; body} -> (
    match compile_non_tail definitions symbol_table stack_index value with
    | Error -> Error
    | Correct value_directives -> (
      match (
        compile_non_tail definitions (Symtab.add name stack_index symbol_table) (stack_index - 8) body
      ) with
      | Error -> Error
      | Correct body_directives -> Correct (
        value_directives @
        [Mov (MemReg (stack_address stack_index, Rax))] @
        body_directives
      )
    )
  )
  | If {conditional; consequent; alternative} -> (
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      match compile_non_tail definitions symbol_table stack_index conditional with
      | Error -> Error
      | Correct conditional_directives -> (
        match compile_non_tail definitions symbol_table stack_index consequent with
        | Error -> Error
        | Correct consequent_directives -> (
          match compile_non_tail definitions symbol_table stack_index alternative with
          | Error -> Error
          | Correct alternative_directives -> Correct (
            conditional_directives @
            [Cmp (RegImm (Rax, encode_bool false)); Jz else_label] @
            consequent_directives @
            [Jmp continue_label] @
            [Label else_label] @
            alternative_directives @
            [Label continue_label]
          )
        )
      )
  )
  | Var name -> compile_var symbol_table name
  | Pair (left, right) -> compile_pair definitions symbol_table stack_index left right
  | Do {first; last} -> (
    List.fold_left
      (
        fun result arg -> (
          match result with
          | Error -> Error
          | Correct directives -> (
            match compile_non_tail definitions symbol_table stack_index arg with
            | Error -> Error
            | Correct arg_directives -> Correct (directives @ arg_directives)
          )
        )
      )
    (Correct [])
    (first @ [last])
  )
  | Call {function_; arguments} -> failwith todo
    (* (
    match Symtab.find_opt function_name definitions with
    | None -> Error
    | Some definition -> (
      if List.length definition.args <> List.length arguments then Error
      else (
        let stack_base = align_stack_index (stack_index + 8) in
        let _, compiled_args_result = (
          List.fold_left
            (
              fun (i, result) argument -> (
                match result with
                | Error -> (i, Error)
                | Correct directives -> (
                  match compile_non_tail definitions symbol_table (stack_base - 8*(i+2)) argument with
                  | Error -> (i, Error)
                  | Correct new_directives -> (
                    i + 1,
                    Correct (
                      directives @
                      new_directives @
                      [Mov (MemReg (stack_address (stack_base - 8*(i+2)), Rax))]
                    )
                  )
                )
              )
            )
            (0, Correct [])
            arguments
        ) in
        match compiled_args_result with
        | Error -> Error
        | Correct compiled_args -> Correct (
          compiled_args @
          [
            Add (RegImm (Rsp, stack_base));
            Call function_name;
            Sub (RegImm (Rsp, stack_base))
          ]
        )
      )
    )
  ) *)
  | Left arg -> compile_left definitions symbol_table stack_index arg
  | Right arg -> compile_right definitions symbol_table stack_index arg
  | Read_num -> Correct
      [
        Mov (MemReg (stack_address stack_index, Rdi));
        Add (RegImm (Rsp, align_stack_index stack_index));
        Call read_num_function_name;
        Sub (RegImm (Rsp, align_stack_index stack_index));
        Mov (RegMem (Rdi, stack_address stack_index))
      ]
  | New_line -> Correct (compile_new_line stack_index)
  | Print arg -> compile_print definitions symbol_table stack_index arg
)
and compile_not (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct arg_directives -> Correct (arg_directives @ [Cmp (RegImm (Rax, encode_bool false))] @ zf_to_bool)
)
and compile_is_zero (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
  | Error ->Error
  | Correct arg_directives -> Correct (
    arg_directives @
    [Cmp (RegImm (Rax, encode_int 0))] @
    zf_to_bool
  )
)
and compile_is_num (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct arg_directives -> Correct (
      arg_directives @
      [And (RegImm (Rax, get_mask ~shift:num_spec.shift)); Cmp (RegImm (Rax, num_spec.tag))] @
      zf_to_bool
    )
)
and compile_add1 (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct arg_directives -> Correct (
      arg_directives @
      assert_is_number Rax R8 @
      [Add (RegImm (Rax, encode_int 1))]
    )
)
and compile_sub1 (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct arg_directives -> Correct (
      arg_directives @
      assert_is_number Rax R8 @
      [Sub (RegImm (Rax, encode_int 1))]
    )
)
and compile_add (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg1 : lisp_expression) (arg2 : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg1 with
  | Error -> Error
  | Correct arg1_directives -> (
    match compile_non_tail definitions symbol_table (stack_index - 8) arg2 with
    | Error -> Error
    | Correct arg2_directives ->
      let arg1_memory_address = stack_address stack_index in
      Correct (
        arg1_directives @
        assert_is_number Rax R8 @
        [Mov (MemReg (arg1_memory_address, Rax))] @
        arg2_directives @
        assert_is_number Rax R8 @
        [Mov (RegMem (R8, arg1_memory_address)); Add (RegReg (Rax, R8))]
      ) 
  )
)
and compile_sub (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg1 : lisp_expression) (arg2 : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg1 with
    | Error -> Error
    | Correct arg1_directives -> (
      match compile_non_tail definitions symbol_table (stack_index - 8) arg2 with
      | Error -> Error
      | Correct arg2_directives ->
          let arg1_memory_address = stack_address stack_index in
          Correct (
            arg1_directives @
            assert_is_number Rax R8 @
            [Mov (MemReg (arg1_memory_address, Rax))] @
            arg2_directives @
            assert_is_number Rax R8 @
            [Mov (RegMem (R8, arg1_memory_address)); Sub (RegReg (Rax, R8))]
          )
    )
)
and compile_eq (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg1 : lisp_expression) (arg2 : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg1 with
    | Error -> Error
    | Correct arg1_directives -> (
      match compile_non_tail definitions symbol_table (stack_index - 8) arg2 with
      | Error -> Error
      | Correct arg2_directives ->
        let arg1_memory_address = stack_address stack_index in
        Correct (
          arg1_directives @
          [Mov (MemReg (arg1_memory_address, Rax))] @
          arg2_directives @
          [Mov (RegMem (R8, arg1_memory_address)); Cmp (RegReg (Rax, R8))] @
          zf_to_bool
        )
    )
)
and compile_lt (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg1 : lisp_expression) (arg2 : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg1 with
  | Error -> Error
  | Correct arg1_directives -> (
    match compile_non_tail definitions symbol_table (stack_index - 8) arg2 with
    | Error -> Error
    | Correct arg2_directives ->
      let arg1_memory_address = stack_address stack_index in
      Correct (
        arg1_directives @
        assert_is_number Rax R8 @
        [Mov (MemReg (arg1_memory_address, Rax))] @
        arg2_directives @
        assert_is_number Rax R8 @
        [Mov (RegMem (R8, arg1_memory_address)); Cmp (RegReg (R8, Rax))] @
        lf_to_bool
      )
  )
)
and compile_pair (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (left : lisp_expression) (right : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index left with
    | Error -> Error
    | Correct left_directives -> (
      match compile_non_tail definitions symbol_table (stack_index - 8) right with
      | Error -> Error
      | Correct right_directives ->
          let left_memory_address = stack_address stack_index in
          Correct (
            left_directives @
            [Mov (MemReg (left_memory_address, Rax))] @
            right_directives @
            [
              Mov (RegMem (R8, left_memory_address));
              Mov (MemReg (Reg Rdi, R8));
              Mov (MemReg (RegImm (Rdi, 8), Rax));
              Mov (RegReg (Rax, Rdi));
              Or (RegImm (Rax, pair_spec.tag));
              Add (RegImm (Rdi, 16))
            ]
          )
    )
)
and compile_left (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
  | Error -> Error
  | Correct arg_directives -> Correct (
    arg_directives @ [Mov (RegMem (Rax, RegImm (Rax, -pair_spec.tag)))]
  )
)
and compile_right (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
  | Error -> Error
  | Correct arg_directives -> Correct (
    arg_directives @ [Mov (RegMem (Rax, RegImm (Rax, -pair_spec.tag + 8)))]
  )
)
and compile_print (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (arg : lisp_expression) : compile_result = (
  match compile_non_tail definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct arg_directives -> Correct (
      arg_directives @
      [
        Mov (MemReg (stack_address stack_index, Rdi));
        Mov (RegReg (Rdi, Rax));
        Add (RegImm (Rsp, align_stack_index stack_index));
        Call print_function_name;
        Sub (RegImm (Rsp, align_stack_index stack_index));
        Mov (RegMem (Rdi, stack_address stack_index));
        Mov (RegImm (Rax, encode_bool true))
      ]
    )
)

let rec compile_tail (definitions : definition Symtab.t) (symbol_table : int Symtab.t)
(stack_index : int) (expression : lisp_expression) : compile_result = (
  match expression with
  | Number n -> Correct (compile_number n @ [Ret])
  | Boolean b -> Correct (compile_boolean b @ [Ret])
  | Not arg -> (
    match compile_not definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Is_zero arg -> (
    match compile_is_zero definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Is_num arg -> (
    match compile_is_num definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Add1 arg -> (
    match compile_add1 definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Sub1 arg -> (
    match compile_sub1 definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | If {conditional; consequent; alternative} -> (
      let else_label = gensym "else" in
      match compile_non_tail definitions symbol_table stack_index conditional with
      | Error -> Error
      | Correct conditional_directives -> (
        match compile_tail definitions symbol_table stack_index consequent with
        | Error -> Error
        | Correct consequent_directives -> (
          match compile_tail definitions symbol_table stack_index alternative with
          | Error -> Error
          | Correct alternative_directives -> Correct (
            conditional_directives @
            [Cmp (RegImm (Rax, encode_bool false)); Jz else_label] @
            consequent_directives @
            [Label else_label] @
            alternative_directives
          )
        )
      )
  )
  | Add (arg1, arg2) -> (
    match compile_add definitions symbol_table stack_index arg1 arg2 with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Sub (arg1, arg2) -> (
    match compile_sub definitions symbol_table stack_index arg1 arg2 with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Eq (arg1, arg2) -> (
    match compile_eq definitions symbol_table stack_index arg1 arg2 with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Lt (arg1, arg2) -> (
    match compile_lt definitions symbol_table stack_index arg1 arg2 with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Let {name; value; body} -> (
    match compile_non_tail definitions symbol_table stack_index value with
    | Error -> Error
    | Correct value_directives -> (
      match (
        compile_tail definitions (Symtab.add name stack_index symbol_table) (stack_index - 8) body
      ) with
      | Error -> Error
      | Correct body_directives -> Correct (
        value_directives @
        [Mov (MemReg (stack_address stack_index, Rax))] @
        body_directives
      )
    )
  )
  | Var name -> (
    match Symtab.find_opt name definitions with
    | Some _ -> Correct [
      LeaLabel {destination = Register Rax; label = name};
      Or (RegImm (Rax, function_spec.tag));
    ]
    | None -> (
      match compile_var symbol_table name with
      | Error -> Error
      | Correct directives -> Correct (directives @ [Ret])
    )
  )
  | Pair (left, right) -> (
    match compile_pair definitions symbol_table stack_index left right with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Left arg -> (
    match compile_left definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Right arg -> (
    match compile_right definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Read_num -> Correct [Jmp read_num_function_name]
  | New_line -> Correct ((compile_new_line stack_index) @ [Ret])
  | Print arg -> (
    match compile_print definitions symbol_table stack_index arg with
    | Error -> Error
    | Correct directives -> Correct (directives @ [Ret])
  )
  | Do {first; last} -> (
      let compiled_first_result = (
        List.fold_left
        (
          fun result arg -> (
            match result with
            | Error -> Error
            | Correct directives -> (
              match compile_non_tail definitions symbol_table stack_index arg with
              | Error -> Error
              | Correct arg_directives -> Correct (directives @ arg_directives)
            )
          )
        )
        (Correct [])
        first
      ) in
      match compiled_first_result with
      | Error -> Error
      | Correct compiled_first -> (
        match compile_tail definitions symbol_table stack_index last with
        | Error -> Error
        | Correct compiled_last -> Correct (compiled_first @ compiled_last)
      )
  )
  | Call {function_; arguments} -> failwith todo
    (* (
    match compile_non_tail definitions symbol_table stack_index function_ with
    | Error -> Error
    | Correct function_directives -> (
      match Symtab.find_opt function_name definitions with
    | None -> Error
    | Some definition -> (
      if List.length definition.args <> List.length arguments then Error
      else (
        let _, compiled_args_result = (
          List.fold_left
            (
              fun (i, result) argument -> (
                match result with
                | Error -> (i, Error)
                | Correct directives -> (
                  match compile_non_tail definitions symbol_table (stack_index - 8*i) argument with
                  | Error -> (i, Error)
                  | Correct new_directives -> (
                    i + 1,
                    Correct (
                      directives @
                      new_directives @
                      [Mov (MemReg (stack_address (stack_index - 8*i), Rax))]
                    )
                  )
                )
              )
            )
            (0, Correct [])
            arguments
        ) in
        match compiled_args_result with
        | Error -> Error
        | Correct compiled_args -> (
          let arg_movement = (
            arguments
            |> List.mapi (fun i _ -> [
              Mov (RegMem (R8, stack_address (stack_index - 8*i)));
              Mov (MemReg (stack_address (-8 * (i+1)), R8))
            ])
            |> List.concat
          ) in
          Correct (compiled_args @ arg_movement @ [Jmp function_name])
        )
      )
    )
    )
    
  ) *)
)

let compile_definitions (definitions : definition Symtab.t) : compile_result = (
  Symtab.fold
    (
      fun name definition result -> (
        match result with
        | Error -> Error
        | Correct directives -> (
          let arg_addresses = List.mapi (fun i arg -> (arg, -8 * (i + 1))) definition.args in
          let function_call_env = (
            List.fold_left
              (fun result (name, value) -> Symtab.add name value result )
              Symtab.empty
              arg_addresses
          ) in
          match compile_tail definitions function_call_env (-8 * (List.length definition.args + 1)) definition.body with
          | Error -> Error
          | Correct new_directives -> Correct (directives @ [Label name] @ new_directives)
        )
      )
    )
    definitions
    (Correct [])
)

let compile (program : program) : compile_result = (
  match compile_definitions program.definitions with
  | Error -> Error
  | Correct definitions -> (
    match compile_tail program.definitions Symtab.empty (-8) program.body with
    | Error -> Error
    | Correct body -> Correct (
      [
        Global "entry";
        Extern error_function_name;
        Extern read_num_function_name;
        Extern print_newline_function_name;
        Extern print_function_name;
        Label "entry"
      ] @
      body @
      definitions
    )
  )
)

let run (program : program) : string = (
  match compile program with
  | Error -> failwith todo
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
      ignore (Unix.system "gcc program.o runtime.o -o program -z noexecstack") ;
      let inp = Unix.open_process_in "./program" in
      let r = input_line inp in
      close_in inp ;
      r
)
