open System.IO
open Syntax

let read_file filename = File.ReadAllText filename

let tokens =
  BlockTokenizer.get_liquid_tokens (read_file "./test.liquid")

List.iter (fun block -> printfn "<---\n%s\n--->" block.Content) tokens

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

let lexed_result =
  "./test.liquid"
  |> read_file
  |> BlockTokenizer.get_liquid_tokens
  |> Lexer.lex_liquid_blocks
//|> Tree.construct_syntax_tree

print_blocks (lexed_result)

let test_eval statement =
  statement
  |> Lexer.get_tag_contents
  |> Lexer.lex_block
  |> Interpreter.interpret_statement

// printfn "%s" (test_eval "{% assign apple = 10 %}")
// printfn "%s" (test_eval "{% render 'horseradish' with x: 10 %}")
// printfn "%s" (test_eval "{% render 'pearsauce' %}")
// printfn "%s" (test_eval "{%- for winner in winners -%}")

(*
Syntax Tree:
Start If a == 10
  echo a
  Start if a == 10
    echo a
  End endif
End endif

*)

let test_block_tree =
  [ Liquid (Statement, [ If; Identifier "a"; EqEq; Number 10 ]);
    Liquid (Output, [ Identifier "a" ]);
    Liquid (Statement, [ If; Identifier "a"; EqEq; Number 10 ]);
    Liquid (Output, [ Identifier "a" ]);
    Liquid (Statement, [ EndIf ]);
    Liquid (Output, [ Identifier "a" ]);
    Liquid (Statement, [ EndIf ]);
    Liquid (Output, [ Identifier "b" ]) ]

test_block_tree
|> Tree.construct_syntax_tree
|> print_blocks
