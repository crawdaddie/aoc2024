open Util
open Printf

(** output a list of just mul expressions *)
let find_mul_expressions str =
  let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec find_all start result_list =
    try
      let pos = Str.search_forward regex str start in
      let num1 = Str.matched_group 1 str in
      let num2 = Str.matched_group 2 str in
      find_all (pos + 1)
        ((int_of_string num1, int_of_string num2) :: result_list)
    with Not_found -> List.rev result_list
  in
  find_all 0 []

type expression = Mul of int * int | Do | Dont

(** output a list of expressions either Mul(%d,%d) or the commands Do() / Don't() *)
let find_mul_expressions_qualified str =
  let regex =
    Str.regexp "\\(mul(\\([0-9]+\\),\\([0-9]+\\))\\|do\\(n't\\)?()\\)"
  in
  let rec find_all start result_list =
    try
      let pos = Str.search_forward regex str start in
      let match_str = Str.matched_string str in
      let expr =
        if String.sub match_str 0 3 = "mul" then
          let num1 = Str.matched_group 2 str in
          let num2 = Str.matched_group 3 str in
          Mul (int_of_string num1, int_of_string num2)
        else if match_str = "don't()" then Dont
        else Do
      in
      find_all (pos + 1) (expr :: result_list)
    with Not_found -> List.rev result_list
  in
  find_all 0 []

let part1 content =
  let nums = find_mul_expressions content in
  List.fold_left (fun acc (l, r) -> acc + (l * r)) 0 nums

let part2 content =
  let nums = find_mul_expressions_qualified content in

  let acc, _ =
    List.fold_left
      (fun (acc, last_do_expr) expr ->
        match expr with
        | Mul (l, r) when last_do_expr = Do -> (acc + (l * r), last_do_expr)
        | Mul (_, _) when last_do_expr = Dont -> (acc, last_do_expr)
        | Do -> (acc, Do)
        | Dont -> (acc, Dont)
        | _ -> (acc, last_do_expr))
      (0, Do) nums
  in
  acc

let () =
  let content = input_string "bin/inputs/day03.txt" in
  content |> part1 |> printf "part1: %d\n";
  content |> part2 |> printf "part2: %d\n"
