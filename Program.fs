open System.Text.RegularExpressions
open System.IO

printfn "Hello from F#"

let block_regex = "\{\%(\-)?(.+)(-)?\%\}"
let output_regex = "\{\{(?:\s+)?(.+?)(?:\s+)?\}\}"

let read_file filename = File.ReadAllText filename

printfn "%s" (read_file "./test.liquid")
