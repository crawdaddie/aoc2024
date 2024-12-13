(* open Util *)
(* open Printf *)
(**)
(* external defrag_disk : string -> int = "defrag_disk" *)
(* external defrag_disk_intact_files : string -> int = "defrag2" *)
(**)
(* let checksum disk = *)
(*   Array.fold_left *)
(*     (fun (acc, i) v -> *)
(*       match v with 0 -> (acc, i + 1) | v -> (acc + (i * (v - 1)), i + 1)) *)
(*     (0, 0) disk *)
(**)
(* let char_to_int ch = Char.code ch - Char.code '0' *)
(**)
(* let disk_size_in_blocks map = *)
(*   String.fold_left (fun acc ch -> acc + char_to_int ch) 0 map *)
(**)
(* let part1 content = defrag_disk content *)
(* let part2 content = defrag_disk_intact_files content *)
(**)
(* let () = *)
(*   (* let content = input_string "bin/inputs/day09.txt" |> String.trim in *) *)
(*   let content = "2333133121414131402" in *)
(**)
(*   content |> part1 |> printf "part1: %d\n"; *)
(*   content |> part2 |> printf "part2: %d\n" *)
(* 2024 Day 9 *)

(* Input *)
let read_in path =
  let open In_channel in
  with_open_text path input_line
  |> Option.value ~default:""
  |> Fun.flip
       (String.fold_right (fun ch acc ->
            int_of_char ch - 48 |> Fun.flip List.cons acc))
       []

(* Part 1 *)
type block = File of int | Space

type fsys = {
  disk_layout : block list;
  free_spaces : int list;
  file_total_size : int;
}

let file_flagged n = Int.rem n 2 = 0

let expand_disk_map =
  let rec loop idx ptr fid
      ({ disk_layout; free_spaces; file_total_size } as fsys) dskm =
    match dskm with
    | region_size :: rst ->
        if file_flagged idx then
          let region = List.init region_size (File fid |> Fun.const) in
          loop (Int.succ idx) (Int.add ptr region_size) (Int.succ fid)
            {
              fsys with
              disk_layout = List.append disk_layout region;
              file_total_size = file_total_size + region_size;
            }
            rst
        else
          let region = List.init region_size (Fun.const Space)
          and spaces = List.init region_size (Int.add ptr) in
          loop (Int.succ idx) (Int.add ptr region_size) fid
            {
              fsys with
              disk_layout = List.append disk_layout region;
              free_spaces = List.append free_spaces spaces;
            }
            rst
    | _ -> fsys
  in
  loop 0 0 0 { disk_layout = []; free_spaces = []; file_total_size = 0 }

let aset arr idx el =
  try
    Array.set arr idx el;
    arr
  with Invalid_argument _ -> arr

let compact fsys =
  let rec loop disk_actual disk_rev free_spaces =
    match (disk_rev, free_spaces) with
    | head_block :: rst_blocks, head_space :: rst_spaces ->
        if head_block = Space then loop disk_actual rst_blocks free_spaces
        else loop (aset disk_actual head_space head_block) rst_blocks rst_spaces
    | _ -> disk_actual
  in
  loop
    (fsys.disk_layout |> Array.of_list)
    (fsys.disk_layout |> List.rev)
    fsys.free_spaces

let checksum max_size disk_actual =
  let rec loop idx sum =
    if idx >= max_size then sum
    else
      let jdx = Int.succ idx in
      match Array.get disk_actual idx with
      | File id -> loop jdx ((idx * id) + sum)
      | _ -> loop jdx sum
  in
  loop 0 0

let fst_solve path =
  let dkm = read_in path |> expand_disk_map in
  compact dkm |> checksum dkm.file_total_size

(* XXXXXXXXXXXXX *)

(* Part 2 *)
module SectorMap = Map.Make (Int)

type swapsys = {
  swap_layout : block list;
  written_sectors : (int * int * int) list; (* fid, sector size, idx *)
  free_sectors : int SectorMap.t; (* sector start idx, sector size *)
}

let bool_with b f x = if b then f x else x

let expand_swap_map =
  let rec loop idx ptr fid
      ({ swap_layout; written_sectors; free_sectors } as swsys) dskm =
    match dskm with
    | region_size :: rst ->
        if file_flagged idx then
          let region = List.init region_size (File fid |> Fun.const) in
          loop (Int.succ idx) (Int.add ptr region_size) (Int.succ fid)
            {
              swsys with
              swap_layout = List.append swap_layout region;
              written_sectors =
                List.cons (fid, region_size, ptr) written_sectors;
            }
            rst
        else
          let region = List.init region_size (Fun.const Space) in
          loop (Int.succ idx) (Int.add ptr region_size) fid
            {
              swsys with
              swap_layout = List.append swap_layout region;
              free_sectors = SectorMap.add ptr region_size free_sectors;
            }
            rst
    | _ -> swsys
  in
  loop 0 0 0
    { swap_layout = []; written_sectors = []; free_sectors = SectorMap.empty }

let move_fitting_write mem_actual free_ptr write_size write_idx write_fid =
  let rec loop_exn write_to_idx del_from_idx write_run =
    if write_run = 0 then mem_actual
    else (
      Array.set mem_actual write_to_idx (File write_fid);
      Array.set mem_actual del_from_idx Space;
      loop_exn (Int.succ write_to_idx) (Int.pred del_from_idx)
        (Int.pred write_run))
  in
  loop_exn free_ptr (write_idx + write_size - 1) write_size

let find_fitting_space free_sectors ptr_max write_size =
  let rec loop curr_ptr =
    if curr_ptr > ptr_max then None
    else
      match SectorMap.find_opt curr_ptr free_sectors with
      | Some free_size ->
          if write_size <= free_size then
            let free_trunc =
              SectorMap.remove curr_ptr free_sectors
              |> bool_with (write_size < free_size)
                 @@ SectorMap.add (curr_ptr + write_size)
                      (free_size - write_size)
            in
            (curr_ptr, free_trunc) |> Option.some
          else Int.succ curr_ptr |> loop
      | None -> Int.succ curr_ptr |> loop
  in
  loop 0

let vacuum swsys =
  let rec loop written_sectors free_sectors mem_actual =
    match written_sectors with
    | (fid, write_size, write_idx) :: rst_written_sectors -> (
        match find_fitting_space free_sectors write_idx write_size with
        | Some (free_ptr, free_trunc) ->
            move_fitting_write mem_actual free_ptr write_size write_idx fid
            |> loop rst_written_sectors free_trunc
        | None -> loop rst_written_sectors free_sectors mem_actual)
    | _ -> mem_actual
  in
  loop swsys.written_sectors swsys.free_sectors
    (swsys.swap_layout |> Array.of_list)

let checksum_complete =
  Array.fold_left
    (fun (idx, sum) block ->
      match block with
      | File id -> (Int.succ idx, (idx * id) + sum)
      | Space -> (Int.succ idx, sum))
    (0, 0)

let () =
  read_in "bin/inputs/day09.txt"
  |> expand_swap_map |> vacuum |> checksum_complete |> snd
  |> Printf.printf "part2: %d\n"

(* XXXXXXXXXXXXX *)
