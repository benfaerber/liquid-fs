open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let tokens = BlockTokenizer.get_liquid_tokens (read_file "./test.liquid")
// List.iter (fun block -> printfn "<---\n%s\n--->" block.Content) tokens
let lexed, unlexed = LiquidLexer.lex_keyword "else apple = 12"
let blexed, bunlexed = LiquidLexer.lex_bool "false apple = 12"
let slexed, sunlexed = LiquidLexer.lex_string "\"This is a string\" apple = 12"

match slexed with
| Some x -> printfn "%s" (LiquidLexer.token_to_string x)
| None -> printfn "None"
