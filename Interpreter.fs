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

let interpret (ast: node list) = 1
