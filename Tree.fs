module Tree

open Syntax


type node =
  | Block of block
  | Scope of block * node list

let rec syntax_tree_to_string =
  function
  | Block b -> block_to_string b
  | Scope (parent, children) ->
    sprintf
      "Parent (%s) - Children (%s)"
      (block_to_string parent)
      (List.map syntax_tree_to_string children
       |> String.concat ", ")


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

let construct_syntax_tree (blocks: block list) =
  let enumerated_blocks = blocks |> enumerate in

  let rec aux bs =
    List.fold
      (fun acc (index, block) ->
        match block with
        | Text t -> acc @ [ Block (Text t) ]
        | Liquid (bt, tokens) ->
          let is_opener = tokens |> List.head |> is_open_tag in

          if is_opener then
            let close_point =
              find_closing_tag (enumerated_blocks[index..]) in

            acc
            @ [ Scope (
                  block,
                  enumerated_blocks[index..close_point]
                  |> unenumerate
                  |> List.map (fun b -> Block b)
                ) ]
          else
            acc @ [ Block (Liquid (bt, tokens)) ])
      bs in


  let global_scope =
    Scope (Liquid (Statement, [ Capture; Identifier "_global_scope" ]), Block (aux enumerated_blocks)) in

  global_scope
