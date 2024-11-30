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
      Seq.Cons(line, next)
    with End_of_file -> 
      close_in ic;
      Seq.Nil
  in
  next


