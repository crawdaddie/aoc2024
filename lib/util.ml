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
