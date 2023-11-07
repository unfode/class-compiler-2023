let gensym : string -> string = (
  let counter = ref 0 in
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1 ;
    symbol
)

module Symtab = Map.Make (struct
  type t = string

  let compare = compare
end)

let rec get_first_last (lst: 'a list) : 'a list * 'a = (
  match lst with
  | [] -> failwith ""
  | x :: [] -> ([], x)
  | x :: xs -> (
    let (first, last) = get_first_last xs in
    (x :: first, last)
  )
)

let todo : string = "todo"