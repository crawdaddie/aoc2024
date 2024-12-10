open Util
open Printf

external defrag_disk : string -> int array = "defrag_disk"

let checksum disk =
  Array.fold_left
    (fun (acc, i) v ->
      match v with 0 -> (acc, i + 1) | v -> (acc + (i * (v - 1)), i + 1))
    (0, 0) disk

let part1 content =
  let disk = defrag_disk content in
  let sum, _ = checksum disk in
  sum

let char_to_int ch = Char.code ch - Char.code '0'

let disk_size_in_blocks map =
  String.fold_left (fun acc ch -> acc + char_to_int ch) 0 map

module DiskMap = struct
  type 'a node = {
    id : int;
    space : int;
    mutable prev : 'a node option;
    mutable next : 'a node option;
  }

  let make strmap =
    (* First create all nodes with just their data *)
    let nodes, _ =
      String.fold_left
        (fun (l, i) ch ->
          match i mod 2 with
          | 0 ->
              let id = i / 2 in
              let node =
                {
                  id = id + 1;
                  space = char_to_int strmap.[i];
                  prev = None;
                  next = None;
                }
              in
              (node :: l, i + 1)
          | _ ->
              let node =
                {
                  id = 0;
                  space = char_to_int strmap.[i];
                  prev = None;
                  next = None;
                }
              in
              (node :: l, i + 1))
        ([], 0) strmap
    in
    let tail =
      match nodes with tail :: _ -> tail | _ -> failwith "input error"
    in

    let nodes = List.rev nodes in

    let rec link_nodes = function
      | [] -> ()
      | [ x ] -> ()
      | x :: (y :: _ as rest) ->
          x.next <- Some y;
          y.prev <- Some x;
          link_nodes rest
    in
    link_nodes nodes;

    List.iter (fun node -> printf "[%d (%d)] " node.id node.space) nodes;
    let hd = match nodes with hd :: _ -> hd | _ -> failwith "input error" in
    (hd, tail)

  let print_node node = printf "[%d (%d)] " node.id node.space
  let next node = node.next
  let prev node = node.prev

  let replace_node old_node new_node =
    (* Update the prev links *)
    new_node.prev <- old_node.prev;
    (match old_node.prev with
    | Some prev -> prev.next <- Some new_node
    | None -> ());

    (* Update the next links *)
    new_node.next <- old_node.next;
    (match old_node.next with
    | Some next -> next.prev <- Some new_node
    | None -> ());

    (* Clear old node's links *)
    old_node.prev <- None;
    old_node.next <- None;

    new_node

  let replace_with_sequence old_node head tail =
    (* Connect the sequence's head to old_node's prev *)
    head.prev <- old_node.prev;
    (match old_node.prev with
    | Some prev -> prev.next <- Some head
    | None -> ());

    (* Connect the sequence's tail to old_node's next *)
    tail.next <- old_node.next;
    (match old_node.next with
    | Some next -> next.prev <- Some tail
    | None -> ());

    (* Clear old node's links *)
    old_node.prev <- None;
    old_node.next <- None;

    (head, tail)

  let rec get_next_empty node =
    match node.id with
    | 0 -> Some node
    | _ -> (
        match node.next with Some node -> get_next_empty node | None -> None)
end

let part2 content =
  let hd, tail = DiskMap.make content in

  DiskMap.print_node hd;
  DiskMap.print_node tail;

  (* let disk = Array.make (disk_size_in_blocks content) 0 in *)
  1

let () =
  (* let content = input_string "bin/inputs/day09.txt" |> String.trim in *)
  let content = "2333133121414131402" in

  content |> part1 |> printf "part1: %d\n";
  content |> part2 |> printf "part2: %d"
