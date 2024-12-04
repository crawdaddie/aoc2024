let input_string filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let lines_of_file filename =
  let ic = open_in filename in
  let rec next () =
    try
      let line = input_line ic in
      Seq.Cons (line, next)
    with End_of_file ->
      close_in ic;
      Seq.Nil
  in
  next

let print_int_list l =
  List.iter (fun x -> Printf.printf "%d, " x) l;
  Printf.printf "\n"

let rec any pred l =
  match l with
  | [] -> false
  | hd :: rest -> if pred hd then true else any pred rest

let array_of_string str = str |> String.to_seq |> Array.of_seq

let print_char_matrix m =
  Array.iter
    (fun arr ->
      Array.iter (fun ch -> Printf.printf "%c, " ch) arr;
      Printf.printf "\n")
    m

let matrix_dims matrix = (Array.length matrix, Array.length matrix.(0))

let matrix_fold f m acc =
  let r = ref acc in
  for i = 0 to Array.length m - 1 do
    for j = 0 to Array.length m.(0) - 1 do
      r := f (i, j) m !r
    done
  done;
  !r

let ij_fold f (i_min, i_max) (j_min, j_max) x =
  let r = ref x in
  for i = i_min to i_max do
    for j = j_min to j_max do
      r := f (i, j) !r
    done
  done;
  !r
