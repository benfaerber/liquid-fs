open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let print_lex =
  function
  | Some t, _ -> printfn "Lexed: %s" (token_to_string t)
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
  |> List.map (fun t -> token_to_string t)
  |> String.concat ", "
  |> printfn "%s"

// print_ast (Lexer.lex_block "assign apple = 10")
// print_ast (Lexer.lex_block "product.name | replace: 'dog', 'cat'")

let print_blocks blocks =
  List.iter (fun block -> block |> block_to_string |> printfn "%s") blocks

let generate_syntax_tree_from_file filename =
  filename
  |> read_file
  |> BlockTokenizer.get_liquid_blocks
  |> Lexer.lex_liquid_blocks
  |> Tree.construct_syntax_tree

let print_syntax_tree_from_file filename =
  filename
  |> generate_syntax_tree_from_file
  |> List.map (fun x -> x |> Tree.syntax_tree_to_string)
  |> String.concat "\n\n\n"
  |> printfn "%s"

let interpret_file filename =
  filename
  |> generate_syntax_tree_from_file
  |> Interpreter.interpret
// print_syntax_tree_from_file "./liquid/simple_test.liquid"

// Interpreter.test ()
interpret_file "./liquid/simple_test.liquid"
