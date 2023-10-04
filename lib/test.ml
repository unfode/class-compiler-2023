(* open Compile
   open Interp
   open Lisp_expression *)

(* let difftest (examples : string list) =
   let expression_options = List.map (fun str -> str |> parse |> s_exp_to_lisp_expression) in
   if (List.for_all (fun expression_option -> expression_option <> None) expression_options) then
   let results = List.map (fun ex -> (run ex, interp ex)) examples in
   List.for_all (fun (r1, r2) -> r1 = r2) results
   else failwith "has invalid program" *)

(* let test () = difftest ["43"; "(add1 (add1 3))"; "(sub1 4)"] *)

(* let difftest (expressions : lisp_expression list) =
     let results =
       List.map
         (fun ex -> (run ex, interpret_result_to_string (interpret ex)))
         expressions
     in
     List.for_all (fun (r1, r2) -> r1 = r2) results

   let test () =
     difftest [Number 43; Add1 (Add1 (Number 3)); Sub1 (Number 4)] *)
