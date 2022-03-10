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
  | 1 -> Filters.OneParam
  | 2 -> Filters.TwoParam
  | 3 -> Filters.ThreeParam
  | _ -> Filters.OneParam


let get_filter fs p_count =
  Filters.get (fs |> List.head, get_param_count p_count)

let run_func =
  function
  | Value v :: Pipe :: Identifier func_name :: Colon :: Value p1 :: Comma :: Value p2 :: tl ->
    let filter =
      match get_filter func_name 3 with
      | Filters.Triple f -> f
      | _ -> raise (System.ArgumentException "Bad Argument Count") in

    filter v p1 p2
  | Value v :: Pipe :: Identifier func_name :: Colon :: Value p1 :: tl ->
    let filter =
      match get_filter func_name 2 with
      | Filters.Double f -> f
      | _ -> raise (System.ArgumentException "Bad Argument Count") in

    filter v p1
  | Value v :: Pipe :: Identifier func_name :: tl ->
    let filter =
      match get_filter func_name 1 with
      | Filters.Single f -> f
      | _ -> raise (System.ArgumentException "Bad Argument Count") in

    filter v
  | _ -> NilValue


let interpret (ast: node list) = 1
