module BlockTokenizer

open System.Text.RegularExpressions

open Syntax

type liquid_block =
  { Start: int
    Content: string
    Inside: string }


let string_of_liquid_block =
  function
  | { Start = s; Content = c; Inside = i } -> sprintf "{ Start=\"%d\"; Content=\"%s\"; Inside=\"%s\"; }" s c i

let print_liquid_block lb =
  lb |> string_of_liquid_block |> printfn "%s"

let get_liquid_blocks (matches: MatchCollection) =
  List.map
    (fun (m: Match) ->
      match m.Groups |> Seq.toList with
      | content :: inside :: _ ->
        { Start = m.Index
          Content = content.Value
          Inside = inside.Value }
      | _ -> raise (System.ArgumentException ("Bad Match!")))
    (matches |> Seq.toList)

let get_liquid_range =
  List.map (fun lb -> (lb.Start, lb.Start + lb.Content.Length))

let in_liquid_range index lr =
  match List.tryFind (fun (start_pos, end_pos) -> index >= start_pos && index <= end_pos) lr with
  | Some _ -> true
  | _ -> false

let get_non_liquid (text: string) lr =
  let chrs =
    List.mapi (fun index chr -> index, chr) (text |> Seq.toList) in

  let liquid_delim = "---LIQ" in

  let built =
    List.fold
      (fun (acc: string) (i, c) ->
        match in_liquid_range i lr with
        | true when acc.EndsWith liquid_delim -> acc
        | true -> acc + liquid_delim
        | false -> acc + c.ToString ())
      ""
      chrs in

  built.Split liquid_delim |> Array.toList


let get_liquid_tokens raw_liquid =
  let matches: MatchCollection =
    Regex.Matches (raw_liquid, both_regex) in

  let liquid_blocks = get_liquid_blocks matches in

  let liquid_range = get_liquid_range liquid_blocks in

  let content_blocks =
    get_non_liquid raw_liquid liquid_range in

  let paired =
    List.map2
      (fun l_block c_block -> l_block, c_block)
      ({ Start = 0; Content = ""; Inside = "" }
       :: liquid_blocks)
      content_blocks in

  let tokens =
    List.fold
      (fun acc (l_block, c_block) ->
        acc
        @ [ { Content = l_block.Content
              IsLiquid = true }
            { Content = c_block; IsLiquid = false } ])
      []
      paired in

  tokens
