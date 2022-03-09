module Tree

open Syntax

type scope = { Parent: block; Children: block list }

type tree = Branch of tree list

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
let unenumerate collection = List.map (fun (i, v) -> v) collection

let construct_syntax_tree (blocks: block list) =
  let wrapped_blocks =
    [ Liquid (Statement, [ Capture; Identifier "_global_scope" ]) ]
    @ blocks @ [ Liquid (Statement, [ EndCapture ]) ]

  let enumerated_blocks = wrapped_blocks |> enumerate in

  let folder acc (index, block) = 1
  List.fold folder
