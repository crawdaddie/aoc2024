open Util
open Printf

let mmul m p = p

let solve section =
  match section with
  | [ [ a0; a1 ]; [ b0; b1 ]; [ p0; p1 ] ] ->
      let det = Vec2.det (a0, a1) (b0, b1) in
      if det = 0 then None
      else
        let a, b = Vec2.inverse (a0, a1) (b0, b1) in
        let push_a = (fst a * p0) + (fst b * p1) in
        let push_b = (snd a * p0) + (snd b * p1) in
        if push_a mod det = 0 && push_b mod det = 0 then
          Some ((3 * push_a / det) + (push_b / det))
        else None
  | _ -> failwith "invalid section input\n"

let part1 sections =
  List.fold_left
    (fun acc section ->
      match solve section with Some x -> acc + x | None -> acc)
    0 sections

let solve2 section =
  match section with
  | [ [ a0; a1 ]; [ b0; b1 ]; [ p0; p1 ] ] ->
      let det = Vec2.det (a0, a1) (b0, b1) in
      if det = 0 then None
      else
        let p0 = 10000000000000 + p0 in
        let p1 = 10000000000000 + p1 in

        let a, b = Vec2.inverse (a0, a1) (b0, b1) in
        let push_a = (fst a * p0) + (fst b * p1) in
        let push_b = (snd a * p0) + (snd b * p1) in

        if push_a mod det = 0 && push_b mod det = 0 then
          Some ((3 * push_a / det) + (push_b / det))
        else None
  | _ -> failwith "invalid section input\n"

let part2 sections =
  List.fold_left
    (fun acc section ->
      match solve2 section with Some x -> acc + x | None -> acc)
    0 sections

let () =
  let lines = List.of_seq @@ lines_of_file "bin/inputs/day13.txt" in

  let sections =
    lines |> split_list_at_pred (( = ) "") |> List.map (List.map find_ints)
  in
  sections |> part1 |> printf "part1: %d\n";
  sections |> part2 |> printf "part2: %d\n"
(* lines |> part2 |> printf "part2: %d\n" *)
