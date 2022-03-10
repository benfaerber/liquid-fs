module Interpreter

open Syntax

type execution_context = Map<string list, token>

let context =
  Map.empty.Add ([ "enviroment" ], String "Liquid F#")

(*
What can happen?

Assign / Increment / Decrement - The variable context is updated and passed to the next statement
If / Else / Unless - A condition is checked from the variable context, a new scope is run
Case - A variable is looked up from the variable context, pattern matching is applies

Pipe - A function is called (lexical token is passed in)

*)

let get_param_count =
  function
  | Identifier _ :: Pipe :: Identifier _ :: Colon :: Value _ :: Comma :: Value _ :: tl -> 3
  | Identifier _ :: Pipe :: Identifier _ :: Colon :: Value _ :: tl -> 2
  | Identifier _ :: Pipe :: Identifier _ :: tl -> 1
  | _ -> 0


let interpret (ast: node list) = 1
