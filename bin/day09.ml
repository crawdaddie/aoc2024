open Util
open Printf

let char_to_int ch = int_of_char ch - int_of_char '0'

let part1 content =
  let len = String.length content in

  let max_id = len / 2 in
  let rec pack_disk disk id =
    match id with
    | id when id = max_id ->
        let num_blocks = char_to_int content.[id * 2] in
        let chunk = Array.make num_blocks (id + 1) in
        chunk :: disk
    | id ->
        let num_blocks = char_to_int content.[id * 2] in
        let spaces = char_to_int content.[1 + (id * 2)] in
        let chunk =
          Array.append (Array.make num_blocks (id + 1)) (Array.make spaces 0)
        in

        pack_disk (chunk :: disk) (id + 1)
    | _ -> failwith ""
  in
  let disk = Array.concat (List.rev (pack_disk [] 0)) in
  let rec defrag l r =
    match (l, r) with
    | l, r when l >= r -> ()
    | l, r ->
        let idl = disk.(l) - 1 in
        let idl_len = char_to_int content.[idl * 2] in
        let num_spaces = char_to_int content.[1 + (idl * 2)] in
        let next_space = l + idl_len in
        printf "idl: %d len: %d spaces: %d\n" idl idl_len next_space;

        defrag (l + 1) (r - 1)
    | _ -> failwith ""
  in

  defrag 0 (Array.length disk);

  Array.iter
    (fun x -> match x with 0 -> printf "." | x -> printf "[%d]" (x - 1))
    disk;
  1

let part2 content = 1

let () =
  (* let content = input_string "bin/inputs/day09.txt" |> String.trim in *)
  let content = "2333133121414131402" in
  (* printf "00...111...2...333.44.5555.6666.777.888899\n"; *)
  let _ = content |> part1 in
  ()
