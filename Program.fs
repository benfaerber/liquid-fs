[<EntryPoint>]
let main (args) =
  let filename = args |> Array.toList |> List.tryHead in

  match filename with
  | Some fname ->
    Liquid.render_file fname
    0
  | None ->
    printfn "No liquid file provided!"
    1
