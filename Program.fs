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

let lexed_result =
  "./simple_test.liquid"
  |> read_file
  |> BlockTokenizer.get_liquid_blocks
  |> BlockTokenizer.print_simple_blocks
//|> Tree.construct_syntax_tree

// let test_eval statement =
//   statement
//   |> Lexer.get_tag_contents
//   |> Lexer.lex_block
//   |> Interpreter.interpret_statement
// printfn "%s" (test_eval "{% assign apple = 10 %}")
// printfn "%s" (test_eval "{% render 'horseradish' with x: 10 %}")
// printfn "%s" (test_eval "{% render 'pearsauce' %}")
// printfn "%s" (test_eval "{%- for winner in winners -%}")

(*
  As Code:

  if counter == 10
    echo banana

    if counter2 == 10
      echo horse
    endif

    echo counter
  endif

  echo final
*)

// let test_block_tree =
//   [ Liquid (
//       Statement,
//       [ If;
//         Identifier "counter";
//         EqEq;
//         Number 10 ]
//     );
//     Liquid (Output, [ Identifier "banana" ]);
//     Liquid (
//       Statement,
//       [ If;
//         Identifier "counter2";
//         EqEq;
//         Number 10 ]
//     );
//     Liquid (Output, [ Identifier "horse" ]);
//     Liquid (Output, [ Identifier "sheep" ]);

//     Liquid (Statement, [ EndIf ]);
//     Liquid (Output, [ Identifier "counter" ]);
//     Liquid (Statement, [ EndIf ]);
//     Liquid (Output, [ Identifier "final" ]) ]

// test_block_tree
// |> Tree.construct_syntax_tree
// |> List.map (fun x -> x |> Tree.syntax_tree_to_string)
// |> String.concat "\n\n\n"
// |> printfn "%s"

"./simple_test.liquid"
|> read_file
|> BlockTokenizer.get_liquid_blocks
|> Lexer.lex_liquid_blocks
|> Tree.construct_syntax_tree
|> List.map (fun x -> x |> Tree.syntax_tree_to_string)
|> String.concat "\n\n\n"
|> printfn "%s"
