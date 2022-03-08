open System.IO

let read_file filename = File.ReadAllText filename

let tokens = BroadTokenizer.get_liquid_tokens (read_file "./test.liquid")
List.iter (fun a -> printfn "<---\n%s\n--->" a) tokens
