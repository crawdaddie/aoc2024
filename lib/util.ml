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
let char_is_digit char = match char with '0' .. '9' -> true | _ -> false

let find_ints str =
  let len = String.length str in

  let rec find_non_digit idx =
    if idx = len then idx
    else
      match char_is_digit str.[idx] with
      | true -> find_non_digit (idx + 1)
      | false -> idx
  in

  let rec search idx l mul =
    match idx with
    | idx when idx = len -> l
    | idx ->
        let char = str.[idx] in
        if char = '-' then search (idx + 1) l (-1)
        else if char_is_digit char then
          let next_idx = find_non_digit idx in
          let v = int_of_string @@ String.sub str idx (next_idx - idx) in
          search next_idx ((mul * v) :: l) 1
        else search (idx + 1) l mul
  in

  let l = search 0 [] 1 in
  List.rev l

let split_list_at_pred pred lst =
  let rec helper current_group groups = function
    | [] ->
        if current_group = [] then groups else List.rev current_group :: groups
    | x :: xs ->
        if pred x then helper [] (List.rev current_group :: groups) xs
        else helper (x :: current_group) groups xs
  in
  List.rev (helper [] [] lst)

(* let print_char_matrix m = *)
(*   Array.iter *)
(*     (fun arr -> *)
(*       Array.iter (fun ch -> Printf.printf "%c, " ch) arr; *)
(*       Printf.printf "\n") *)
(*     m *)

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

let split_on_empty_line content =
  let lines = String.split_on_char '\n' content in
  let rec split_sections lines section_a section_b after =
    match lines with
    | [] -> (List.rev section_a, List.rev section_b)
    | "" :: rest -> split_sections rest section_a section_b true
    | str :: rest when after ->
        split_sections rest section_a (str :: section_b) true
    | str :: rest when not after ->
        split_sections rest (str :: section_a) section_b false
    | _ -> failwith ""
  in
  split_sections lines [] [] false

let char_matrix content =
  let lines = content |> String.trim |> String.split_on_char '\n' in
  let matrix = Array.of_list (List.map array_of_string lines) in
  matrix

let ch_matrix_find ch matrix =
  let m, n = matrix_dims matrix in
  let rec loop i j =
    if i = m then None
    else if j = n then loop (succ i) 0
    else if matrix.(i).(j) = ch then Some (i, j)
    else loop i (succ j)
  in
  loop 0 0

let matrix_find_all f matrix =
  matrix_fold
    (fun (i, j) map l ->
      match f map.(i).(j) with true -> (i, j) :: l | false -> l)
    matrix []

let ch_matrix_find_exn ch matrix =
  let m, n = matrix_dims matrix in
  let rec loop i j =
    if i = m then failwith "ch not founc"
    else if j = n then loop (succ i) 0
    else if matrix.(i).(j) = ch then (i, j)
    else loop i (succ j)
  in
  loop 0 0

let print_char_matrix matrix =
  let m, n = matrix_dims matrix in
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      Printf.printf "%c" matrix.(i).(j)
    done;
    Printf.printf "\n"
  done

let int_log_10 x =
  let rec aux v c =
    match v with v when v > 0 -> aux (v / 10) c + 1 | _ -> c
  in
  aux x 0

let pow a exp =
  match exp with
  | 0 -> 1
  | 1 -> a
  | n -> int_of_float (Float.of_int a ** Float.of_int exp)

let concat_numbers a b =
  let zeros = int_log_10 b in

  let res = (a * pow 10 zeros) + b in
  (* printf "concat %d %d %d (zeros: %d) \n" a b res zeros; *)
  res

let is_in_bounds (i, j) (m, n) = i < m && i >= 0 && j < n && j >= 0
let in_bounds (m, n) (i, j) = i < m && i >= 0 && j < n && j >= 0

let ordered_pairs lst =
  match lst with
  | [] | [ _ ] -> []
  | _ ->
      List.concat_map
        (fun i ->
          List.filter_map (fun j -> if i <> j then Some (i, j) else None) lst)
        lst

let vadd (i, j) (k, l) = (i + k, j + l)
let vscalar_mul (i, j) a = (i * a, j * a)
let vdot (i, j) (k, l) = (i * k, j * l)
let char_to_int ch = Char.code ch - Char.code '0'

module Vec2 : sig
  type t = int * int

  val compare : t -> t -> int
  val ( +| ) : t -> t -> t
  val ( -| ) : t -> t -> t
  val ( ~| ) : t -> t
  val ( *| ) : int -> t -> t
  val norm_manhattan : t -> int
  val dist_manhattan : t -> t -> int
  val dot : t -> t -> int

  val det : t -> t -> int
  (** Determinant of the 2-2 matrix made of both vectors *)

  val inverse : t -> t -> t * t
  (** Inverse of the matrix (not multiplied by 1/det!) *)

  val pp : Format.formatter -> t -> unit
end = struct
  type t = int * int

  let compare (x, y) (x', y') =
    let cmp = Int.compare x x' in
    if cmp = 0 then Int.compare y y' else cmp

  let ( +| ) (x, y) (x', y') = (x + x', y + y')
  let ( -| ) (x, y) (x', y') = (x - x', y - y')
  let ( ~| ) (x, y) = (-x, -y)
  let ( *| ) n (x, y) = (n * x, n * y)
  let norm_manhattan (x, y) = abs x + abs y
  let dist_manhattan a b = a -| b |> norm_manhattan
  let pp fmt (x, y) = Format.fprintf fmt "(%d,%d)" x y
  let dot (x, y) (x', y') = (x * x') + (y * y')
  let det (x, y) (x', y') = (x * y') - (y * x')
  let inverse (x, y) (x', y') = ((y', -y), (-x', x))
end

module type Num = sig
  type t

  val to_string : t -> string
  val zero : t
  val succ : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val rem : t -> t -> t
  val div : t -> t -> t
  val compare : t -> t -> int
end

module type MATH = sig
  type t

  val gcd : t -> t -> t
  val egcd : t -> t -> t * t * t
  val lcm : t -> t -> t
  val solve_crt : (t * t) list -> t * t
end

module MathGen (N : Num) : MATH with type t = N.t = struct
  type t = N.t

  let one = N.succ N.zero

  let egcd a b =
    let rec loop old_r r old_s s old_t t =
      if N.(compare r zero = 0) then (old_s, old_t, old_r)
      else
        let q = N.div old_r r in
        loop r
          N.(sub old_r (mul q r))
          s
          N.(sub old_s (mul q s))
          t
          N.(sub old_t (mul q t))
    in
    N.(loop a b one zero zero one)

  let gcd a b =
    let _, _, r = egcd a b in
    r

  let lcm a b = N.(mul a (div b (gcd a b)))

  let rec fix a n =
    if N.(compare n a <= 0) then N.rem a n
    else if N.(compare a (neg n) < 0) then fix N.(rem a n) n
    else if N.(compare a zero < 0) then fix N.(add a n) n
    else a

  let rec solve_crt l =
    match l with
    | [] -> failwith "solve_crt"
    | [ (a, n) ] -> (fix a n, n)
    | (a1, n1) :: (a2, n2) :: ll ->
        let m1, m2, x = egcd n1 n2 in
        (if not N.(compare x one = 0) then
           let s1 = N.to_string n1 in
           let s2 = N.to_string n2 in
           raise
             (Invalid_argument
                Printf.(
                  sprintf
                    "coefficient %s and %s are not coprime (gcd(%s, %s)=%s)" s1
                    s2 s1 s2 (N.to_string x))));
        let n12 = N.mul n1 n2 in
        let k1 = N.(mul a1 (mul m2 n2)) in
        let k2 = N.(mul a2 (mul m1 n1)) in
        let a12 = N.add k1 k2 in
        solve_crt ((a12, n12) :: ll)
end

module Math = MathGen (Int)
