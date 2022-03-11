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

let raise_arg_error count =
  raise (System.ArgumentException (sprintf "Expected a function with %d arguments!" count))

let evaluate_filter =
  function
  | [ Value v; Pipe; Identifier func_name; Colon; Value p1; Comma; Value p2 ] ->
    let filter =
      match Filter.get func_name 3 with
      | Filter.ThreeArgument f -> f
      | _ -> raise_arg_error 3 in

    filter v p1 p2
  | [ Value v; Pipe; Identifier func_name; Colon; Value p1 ] ->
    let filter =
      match Filter.get func_name 2 with
      | Filter.TwoArgument f -> f
      | _ -> raise_arg_error 2 in

    filter v p1
  | [ Value v; Pipe; Identifier func_name ] ->
    let filter =
      match Filter.get func_name 1 with
      | Filter.OneArgument f -> f
      | _ -> raise_arg_error 1 in

    filter v
  | _ -> NilValue


let test () =
  let alpha_list =
    List (
      "abcdefghi"
      |> Seq.toList
      |> List.map (fun c -> String (c.ToString ()))
    )

  let tests =
    [ "apply_capitalize",
      [ Value (String "ben");
        Pipe;
        Identifier [ "capitalize" ] ];

      "apply_remove",
      [ Value (String "hello world");
        Pipe;
        Identifier [ "remove" ];
        Colon;
        Value (String " ") ];

      "split by comma",
      [ Value (String "a, b, c, d");
        Pipe;
        Identifier [ "split" ];
        Colon;
        Value (String ", ") ];


      "slice array (1 argument)",
      [ Value alpha_list;
        Pipe;
        Identifier [ "slice" ];
        Colon;
        Value (Number 2) ];

      "slice array (2 arguments)",
      [ Value alpha_list;
        Pipe;
        Identifier [ "slice" ];
        Colon;
        Value (Number 1);
        Comma;
        Value (Number 4) ] ]

  tests
  |> List.iter (fun (name, tokens) ->
    printfn
      "%s:\n  %s"
      name
      (tokens
       |> evaluate_filter
       |> liquid_value_to_string))

let interpret (ast: node list) = 1
