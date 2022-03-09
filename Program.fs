open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let tokens = BlockTokenizer.get_liquid_tokens (read_file "./test.liquid")
// List.iter (fun block -> printfn "<---\n%s\n--->" block.Content) tokens

let print_lex =
  function
  | Some t, _ -> printfn "Lexed: %s" (Lexer.token_to_string t)
  | None, _ -> printfn "Lex Failed!"

print_lex (Lexer.lex_keyword "else apple = 12")
print_lex (Lexer.lex_bool "false apple = 12")
print_lex (Lexer.lex_string "\"This is a string\" apple = 12")
print_lex (Lexer.lex_keyword ">= 12")
print_lex (Lexer.lex_number "-100.84= 12")
print_lex (Lexer.lex_token "x")
print_lex (Lexer.lex_token "|")
print_lex (Lexer.lex_token "(1..10) pear")


print_lex (Lexer.lex_token "apple = 10")
