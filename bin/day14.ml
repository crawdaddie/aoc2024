open Util
open Printf
open Owl

let modneg x m =
  let res = x mod m in
  if res < 0 then m + res else res

let move_robot mul start v =
  let open Vec2 in
  start +| (mul *| v)

let is_in (xmin, xmax, ymin, ymax) (i, j) =
  i >= xmin && i <= xmax && j >= ymin && j <= ymax

let min_index_by f lst =
  let indexed = List.mapi (fun i x -> (i, x)) lst in
  match indexed with
  | [] -> None
  | (i0, x0) :: rest ->
      let min_so_far =
        List.fold_left
          (fun (min_i, min_x) (i, x) ->
            if f x < f min_x then (i, x) else (min_i, min_x))
          (i0, x0) rest
      in
      Some (fst min_so_far)

let min_by f lst =
  List.fold_left (fun min_x x -> if f x < f min_x then x else min_x) 1000. lst

let get_dims lines =
  let n, m =
    List.fold_left
      (fun (max_x, max_y) [ x; y; _; _ ] ->
        let max_x = if x > max_x then x else max_x in
        let max_y = if y > max_y then y else max_y in
        (max_x, max_y))
      (0, 0) lines
  in
  let m, n = (m + 1, n + 1) in
  (m, n)

let part1 (m, n) lines =
  let get_quadrant (m, n) (i, j) =
    let res =
      if i < m / 2 && j < n / 2 then 0
      else if i < m / 2 && j > n / 2 then 1
      else if i > m / 2 && j < n / 2 then 2
      else if i > m / 2 && j > n / 2 then 3
      else -1
    in
    res
  in
  let move (s0, s1) (v0, v1) n =
    let p0, p1 = move_robot 100 (s0, s1) (v0, v1) in
    let p0 = modneg p0 m in
    let p1 = modneg p1 n in
    (p0, p1)
  in

  let q0, q1, q2, q3 =
    List.fold_left
      (fun (q0, q1, q2, q3) l ->
        match l with
        | [ s1; s0; v1; v0 ] -> (
            let p0, p1 = move (s0, s1) (v0, v1) 100 in
            match get_quadrant (m, n) (p0, p1) with
            | 0 -> (q0 + 1, q1, q2, q3)
            | 1 -> (q0, q1 + 1, q2, q3)
            | 2 -> (q0, q1, q2 + 1, q3)
            | 3 -> (q0, q1, q2, q3 + 1)
            | _ -> (q0, q1, q2, q3))
        | _ -> failwith "invalid input")
      (0, 0, 0, 0) lines
  in
  printf "quadrant counts: %d %d %d %d\n" q0 q1 q2 q3;
  q0 * q1 * q2 * q3

(* let part2 (m, n) lines = *)
(*   let move arr = *)
(*     let p0, p1 = move_robot 1 (arr.(1), arr.(0)) (arr.(3), arr.(2)) in *)
(*     let p0 = modneg p0 m in *)
(*     let p1 = modneg p1 n in *)
(*     arr.(1) <- p0; *)
(*     arr.(0) <- p1; *)
(*     arr *)
(*   in *)
(*   let state = Array.of_list (List.map Array.of_list lines) in *)
(*   let size = Float.of_int (Array.length state) in *)
(*   let compute_variance state = *)
(*     let avg = *)
(*       Array.fold_left *)
(*         (fun (accx, accy) [| x; y; _; _ |] -> (accx + x, accy + y)) *)
(*         (0, 0) state *)
(*     in *)
(*     let avgx, avgy = *)
(*       (Float.of_int (fst avg) /. size, Float.of_int (snd avg) /. size) *)
(*     in *)
(*     (* Calculate sum of squared differences from mean *) *)
(*     let sum_sq_diff = *)
(*       Array.fold_left *)
(*         (fun (accx, accy) [| x; y; _; _ |] -> *)
(*           let dx = Float.of_int x -. avgx in *)
(*           let dy = Float.of_int y -. avgy in *)
(*           (accx +. (dx *. dx), accy +. (dy *. dy))) *)
(*         (0.0, 0.0) state *)
(*     in *)
(*     (* Variance is the average of squared differences *) *)
(*     (fst sum_sq_diff /. size, snd sum_sq_diff /. size) *)
(*   in *)
(*   (* let img = Array.init (m + 1) (fun i -> Array.init (n + 1) (fun j -> '.')) in *) *)
(*   let rec loop i l = *)
(*     match i with *)
(*     | 200 -> l *)
(*     | i -> *)
(*         Array.iter *)
(*           (fun arr -> *)
(*             let _arr = move arr in *)
(*             ()) *)
(*           state; *)
(*         let v0, v1 = compute_variance state in *)
(*         printf "%f,%f\n" v0 v1; *)
(*         loop (i + 1) ((v0, v1) :: l) *)
(*   in *)
(*   1 *)
open Owl

let part2 (m, n) lines =
  let open Dense in
  let move n arr =
    let p0, p1 = move_robot n (arr.(1), arr.(0)) (arr.(3), arr.(2)) in
    let p0 = modneg p0 m in
    let p1 = modneg p1 n in
    arr.(1) <- p0;
    arr.(0) <- p1;
    arr
  in

  let state = Array.of_list (List.map Array.of_list lines) in

  let _NUM_VARIANCES = 500 in
  let variances = Ndarray.D.create [| _NUM_VARIANCES; 2 |] 0. in

  let compute_variance state =
    let coords =
      Array.map
        (fun arr -> [| float_of_int arr.(0); float_of_int arr.(1) |])
        state
    in
    let mat = Mat.of_arrays coords in
    let var = Mat.var ~axis:0 mat in
    (Mat.get var 0 0, Mat.get var 0 1)
  in

  let rec loop i =
    match i with
    | i when i = _NUM_VARIANCES -> ()
    | i ->
        Array.iter
          (fun arr ->
            let _arr = move 1 arr in
            ())
          state;
        let v0, v1 = compute_variance state in
        Arr.set variances [| i; 0 |] v0;
        Arr.set variances [| i; 1 |] v1;
        loop (i + 1)
  in
  loop 0;

  let min_variance vars d =
    Arr.get (Arr.get_slice [ []; [ d ] ] vars |> Arr.min) [| 0 |]
  in

  let min_indices vars d =
    let min_val = min_variance vars d in
    let vals = Arr.get_slice [ []; [ d ] ] variances in

    let min_indices =
      Arr.filter (fun x -> abs_float (x -. min_val) < 1e-10) vals
    in
    min_indices
  in

  Array.iter (fun x -> printf "%d, " x) (min_indices variances 0);
  printf "\n";
  Array.iter (fun x -> printf "%d, " x) (min_indices variances 1);
  printf "\n";
  Arr.print variances;
  1

let get_image (m, n) lines =
  let move num arr =
    let p0, p1 = move_robot num (arr.(1), arr.(0)) (arr.(3), arr.(2)) in
    let p0 = modneg p0 m in
    let p1 = modneg p1 n in
    arr.(1) <- p0;
    arr.(0) <- p1;
    arr
  in

  let state = Array.of_list (List.map Array.of_list lines) in

  let move_all_robots num =
    Array.iter
      (fun arr ->
        let _arr = move num arr in
        ())
      state
  in
  move_all_robots 6668;
  let img = Array.init m (fun i -> Array.init n (fun j -> '.')) in
  Array.iter (fun [| s1; s0; _; _ |] -> img.(s0).(s1) <- '#') state;
  print_char_matrix img;
  1

let () =
  let lines = lines_of_file "bin/inputs/day14.txt" in

  let inputs = lines |> Seq.map find_ints |> List.of_seq in
  let dims = get_dims inputs in

  let _ = inputs |> part2 dims in
  let sol = Math.solve_crt [ (1, 101); (75, 103) ] in
  printf "%d %d\n" (fst sol) (snd sol);

  let _ = get_image dims inputs in
  ()
