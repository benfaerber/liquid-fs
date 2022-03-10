module BlockTokenizer

open System.Text.RegularExpressions

open Syntax

let print_simple_blocks blocks =
  List.iter
    (fun b -> printfn "%s <<<%s>>>\n" (if b.IsLiquid then "Liquid" else "Text") (b.Content.Replace ("\n", " ")))
    blocks

let get_uncompressed_block_list (s: string) =
  let generator inp =
    match inp with
    | "" -> None
    | p ->
      let is_liquid =
        Regex.IsMatch (p, starts_with_regex both_regex) in

      if is_liquid then
        let m = Regex.Match (p, starts_with_regex both_regex) in

        let liquid_statement = m.Groups.[0].Value in

        Some (
          { Content = liquid_statement;
            IsLiquid = true },
          inp[liquid_statement.Length ..]
        )
      else
        let first_char = p |> Seq.toList |> List.head in

        Some (
          { Content = first_char.ToString ();
            IsLiquid = false },
          p[1..]
        ) in

  List.unfold generator s

let compress_liquid_blocks blocks =
  let compressor acc curr =
    let last = acc |> List.rev |> List.head in
    let all_but_last = acc |> List.rev |> List.tail in

    match curr with
    | { Content = c; IsLiquid = true } -> acc @ [ { Content = c; IsLiquid = true } ]
    | { Content = c; IsLiquid = false } ->
      (match last with
       | { Content = _; IsLiquid = true } -> acc @ [ { Content = c; IsLiquid = false } ]
       | { Content = last_content;
           IsLiquid = false } ->
         all_but_last
         @ [ { Content = last_content + c;
               IsLiquid = false } ]) in

  List.fold compressor [ { Content = ""; IsLiquid = false } ] blocks

let get_liquid_blocks (s: string) =
  s
  |> get_uncompressed_block_list
  |> compress_liquid_blocks
