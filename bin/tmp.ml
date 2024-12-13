(* Helper function to safely get grid value, returns None if out of bounds *)
let get_cell grid row col =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  if row >= 0 && row < height && col >= 0 && col < width then
    Some grid.(row).(col)
  else None

(* Check if a cell matches the target character *)
let matches_char cell target =
  match cell with Some c when c = target -> true | _ -> false

(* Main function to check if position is a corner *)
let is_corner grid target_char row col =
  let positions = [ (0, 1); (1, 0); (0, -1); (-1, 0) ] in

  (* right, down, left, up *)

  (* First verify the current position is part of the region *)
  match get_cell grid row col with
  | Some c when c = target_char ->
      (* Get values of adjacent cells *)
      let adjacent =
        List.map
          (fun (dr, dc) ->
            matches_char (get_cell grid (row + dr) (col + dc)) target_char)
          positions
      in

      (* Helper to check if two adjacent positions match *)
      let check_adjacent_pair i =
        matches_char
          (get_cell grid
             (row + fst (List.nth positions i))
             (col + snd (List.nth positions i)))
          target_char
        && matches_char
             (get_cell grid
                (row + fst (List.nth positions ((i + 1) mod 4)))
                (col + snd (List.nth positions ((i + 1) mod 4))))
             target_char
      in

      (* Check all possible corner configurations *)
      let rec is_corner_config i =
        if i >= 4 then false
        else if check_adjacent_pair i then
          (* Count matches in opposite directions *)
          let opposite1 =
            matches_char
              (get_cell grid
                 (row + fst (List.nth positions ((i + 2) mod 4)))
                 (col + snd (List.nth positions ((i + 2) mod 4))))
              target_char
          in
          let opposite2 =
            matches_char
              (get_cell grid
                 (row + fst (List.nth positions ((i + 3) mod 4)))
                 (col + snd (List.nth positions ((i + 3) mod 4))))
              target_char
          in
          (opposite1 && not opposite2)
          || ((not opposite1) && not opposite2)
          || ((not opposite1) && opposite2)
        else is_corner_config (i + 1)
      in

      is_corner_config 0
  | _ -> false
