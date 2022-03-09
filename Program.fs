open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let tokens =
  BlockTokenizer.get_liquid_tokens (read_file "./test.liquid")

List.iter (fun block -> printfn "<---\n%s\n--->" block.Content) tokens

let print_lex =
  function
  | Some t, _ -> printfn "Lexed: %s" (Lexer.token_to_string t)
  | None, _ -> printfn "Lex Failed!"


// print_lex (Lexer.lex_keyword "else apple = 12")
// print_lex (Lexer.lex_bool "false apple = 12")
// print_lex (Lexer.lex_string "\"This is a string\" apple = 12")
// print_lex (Lexer.lex_keyword ">= 12")
// print_lex (Lexer.lex_number "-100.84= 12")
// print_lex (Lexer.lex_token "x")
// print_lex (Lexer.lex_token "|")
// print_lex (Lexer.lex_token "(1..10) pear")


// print_lex (Lexer.lex_token "apple = 10")
let print_ast ast =
  ast
  |> List.map (fun t -> Lexer.token_to_string t)
  |> String.concat ", "
  |> printfn "%s"

// print_ast (Lexer.lex_block "assign apple = 10")
// print_ast (Lexer.lex_block "product.name | replace: 'dog', 'cat'")

let print_blocks blocks =
  List.iter (fun block -> block |> Lexer.block_to_string |> printfn "%s") blocks

let lexed_result =
  "./test.liquid"
  |> read_file
  |> BlockTokenizer.get_liquid_tokens
  |> Lexer.lex_liquid_blocks

print_blocks (lexed_result)
