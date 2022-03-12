module Tree

open Syntax

let is_debug_mode = false

let sleep () =
  if is_debug_mode then
    Async.Sleep (500) |> Async.RunSynchronously

let get_tabs count =
  List.init count (fun _ -> "  ")
  |> String.concat ""

let r = System.Random ()

let random_delims () =
  match r.Next (9) with
  | 1 -> "<", ">"
  | 2 -> "[", "]"
  | 3 -> "(", ")"
  | 4 -> "{", "}"
  | 5 -> "<-", "->"
  | 6 -> "[-", "-]"
  | 7 -> "(-", "-)"
  | 8 -> "{-", "-}"
  | _ -> "(", ")"


let debug_syntax_tree_to_string =
  let rec aux tabs =
    function
    | Block b -> sprintf "%sBlock (%s)" (get_tabs tabs) (debug_block_to_string b)
    | Scope (parent, children) ->
      let open_delim, close_delim = random_delims () in

      sprintf
        "%s\n%s%s\n%s%s\n%s"
        open_delim
        (get_tabs (tabs))
        (debug_block_to_string parent)
        (get_tabs (tabs))

        (List.map (fun c -> aux (tabs + 1) c) children
         |> String.concat (sprintf ",\n%s" (get_tabs tabs)))
        close_delim in aux 0


let is_open_tag =
  function
  | If
  | For
  | Case
  | Unless
  | Comment
  | Capture -> true
  | _ -> false

let get_pair keyword =
  let pairs =
    [ If, EndIf;
      For, EndFor;
      Case, EndCase;
      Unless, EndUnless;
      Comment, EndComment;
      Capture, EndCapture ] in

  let found =
    List.tryFind (fun (kwopen, kwclose) -> keyword = kwopen || keyword = kwclose) pairs in

  match found with
  | Some (kwopen, kwclose) ->
    Some (
      if kwopen = keyword then
        kwclose
      else
        kwopen
    )
  | None -> None

let has_pair keyword =
  match get_pair keyword with
  | Some _ -> true
  | None -> false

let find_closing_tag (enumerated_blocks: (int * block) list) =
  let _, open_block = enumerated_blocks |> List.head in

  let open_tag =
    match open_block with
    | Liquid (_, ts) -> List.head ts
    | _ -> raise (System.ArgumentException ("Text Block has no closing Tag!"))

  let close_tag = (get_pair open_tag).Value in

  let count_current_block current_block l_counter going index =
    match current_block with
    | Text _ -> l_counter, going, index
    | Liquid (_, tokens) ->
      (let block_offset =
        List.fold
          (fun tag_acc tag_curr ->
            let offset =
              match tag_curr with
              | t when t = open_tag -> 1
              | t when t = close_tag -> -1
              | _ -> 0 in

            tag_acc + offset)
          0
          tokens in


       let counter = l_counter + block_offset in


       counter, counter <> 0, index) in

  let folder (l_counter, going, acc) (index, current_block) =
    if not going then
      0, false, acc
    else
      count_current_block current_block l_counter going index in

  let _, _, end_index =
    List.fold folder (0, true, 0) enumerated_blocks in

  end_index

let enumerate collection = List.mapi (fun i v -> i, v) collection
let unenumerate collection = List.map (fun (_, v) -> v) collection

(*

    let folder acc (index, block) =
      match block with
      | Text t -> []
      | Liquid (bt, ts) -> []
*)

let rec construct_syntax_tree (blocks: block list) =
  let enumerated_blocks = blocks |> enumerate in
  let max_index = (blocks |> List.length) - 1 in

  let rec aux start_index end_index =

    List.unfold
      (function
      | index when index > end_index -> None
      | index ->
        sleep ()

        (match blocks.[index] with
         | Text t -> Some (Block (Text t), index + 1)
         | Liquid (_, tokens) ->
           let is_opener = tokens |> List.head |> is_open_tag in

           if is_opener then
             let close_index =
               find_closing_tag (enumerated_blocks[index..]) in

             let scope =
               Scope (blocks.[index], aux (index + 1) (close_index - 1)) in

             Some (scope, close_index + 1)
           else
             Some (Block blocks.[index], index + 1)))
      start_index in

  let main_children = aux 0 max_index in

  let global_scope =
    Liquid (
      Statement,
      [ Capture;
        Identifier [ global_scope_capture ] ]
    ) in

  [ Scope (global_scope, main_children) ]
