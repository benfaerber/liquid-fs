module Liquid

open System.IO
open Syntax

let read_file filename = File.ReadAllText filename
let write_file filename contents = File.WriteAllText (filename, contents)

let tap func inp =
  func inp
  inp


let generate_syntax_tree_from_file filename =
  filename
  |> read_file
  |> BlockTokenizer.get_liquid_blocks
  |> Lexer.lex_liquid_blocks
  |> Tree.construct_syntax_tree

let interpret_file filename =
  filename
  |> generate_syntax_tree_from_file
  //|> tap (fun t -> printfn "%s" (Tree.debug_syntax_tree_to_string (List.head t)))
  |> tap (fun t -> write_file "./liquid/syntax_tree.txt" (Tree.debug_syntax_tree_to_string (List.head t)))
  |> Interpreter.interpret

let render_file filename =
  filename
  |> interpret_file
  |> tap (fun r -> printfn "Rendered Liquid:\n------------\n%s\n------------" (r.Replace ("\n\n", "\n")))
  |> write_file (sprintf "%s-rendered.html" (filename.Replace (".liquid", "")))
