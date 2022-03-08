open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let tokens = BlockTokenizer.get_liquid_tokens (read_file "./test.liquid")
// List.iter (fun block -> printfn "<---\n%s\n--->" block.Content) tokens

let print_lex =
  function
  | Some t, rest -> printfn "Lexed: %s" (LiquidLexer.token_to_string t)
  | None, rest -> printfn "Lex Failed!"

print_lex (LiquidLexer.lex_keyword "else apple = 12")
print_lex (LiquidLexer.lex_bool "false apple = 12")
print_lex (LiquidLexer.lex_string "\"This is a string\" apple = 12")
print_lex (LiquidLexer.lex_operator ">= 12")
print_lex (LiquidLexer.lex_number "-100.84= 12")
print_lex (LiquidLexer.lex_token "x")
