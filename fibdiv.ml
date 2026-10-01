(* Calculate 
      fibdiv : N -> N*
   which finds the highest sum of fibonacci numbers, and repeats recursively for remainder. 

   For example, fibdiv(25) = 6.3.1.0,
   because the highest sum of fibonacci numbers is 20 with index 6, then 3 for remaining 4 out of 5 then 1 then 0.

   n   | 1 2 3 4 5  6  7  ..
   ----+---------------------
   fib | 1 1 2 3 5  8  13 ..
   sum | 1 2 4 7 12 20 33 ..
*)

let fibdiv n = 
  let rec f n acc fibs slice fsi = 
    let gen_fib fib slice = match slice with
      | a::b::[] -> (fibs @ [a + b], [b; a + b])
      | _ -> assert false
    in 
    match n with
      | 0 -> acc @ [fsi] @ [0]
      | n -> if n > List.hd slice then
          let (fibs', slice') = gen_fib fibs slice in
          f (n - List.hd slice) acc fibs' slice' (fsi + 1)
        else if n == List.hd slice then
          acc @ [fsi] @ [0]
        else
          f n (acc @ [fsi - 1]) fibs [1; 1] 1 
  in match n with
    | 0 -> [0]
    | n -> f n [] [1; 1] [1; 1] 1

(* Define a dot separator *)
let pp_sep fmt () = Format.fprintf fmt "."

let () = 
  let n = 45 in
  Format.printf "Fibdiv of %d is %a\n" 
    n
    (Format.pp_print_list ~pp_sep Format.pp_print_int) (fibdiv n)
