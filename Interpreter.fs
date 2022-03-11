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

let rec value_to_string =
  function
  | Boolean b -> if b then "true" else "false"
  | String s -> s
  | Number n -> sprintf "%f" n
  | NilValue -> "nil"
  | EmptyValue -> "empty"
  | List l ->
    l
    |> List.map value_to_string
    |> String.concat ", "


let raise_arg_error count =
  raise (System.ArgumentException (sprintf "Expected a function with %d arguments!" count))

let eval_filter =
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

let rec interpret (ast: node list) =

  let eval_liquid_output_block (outp, ctx) tokens =
    match tokens with
    | _ :: Pipe :: _ as ts -> outp + (ts |> eval_filter |> value_to_string), ctx
    | [ Value v ] -> outp + (value_to_string v), ctx
    | _ -> outp, ctx

  let eval_liquid_statement_block (outp, ctx) tokens = outp, ctx

  let eval_block (outp, ctx) =
    function
    | Text txt -> (outp + txt, ctx)
    | Liquid (Output, tokens) -> eval_liquid_output_block (outp, ctx) tokens
    | Liquid (Statement, tokens) -> eval_liquid_statement_block (outp, ctx) tokens

  let rec eval_scope (outp, ctx) parent children =
    match parent with
    | Liquid (_, tokens) ->
      let scope_type = tokens |> List.head in

      match scope_type with
      | If -> (outp, ctx)
      | For -> (outp, ctx)
      | Case -> (outp, ctx)
      | Unless -> (outp, ctx)
      | Comment -> (outp, ctx)
      | Capture ->
        let eval_folder acc dnode =
          match dnode with
          | Block bl -> eval_block acc bl
          | Scope (sp, sc) -> eval_scope acc sp sc in

        let res =
          children |> List.fold eval_folder (outp, ctx) in

        res
      | _ -> (outp, ctx)
    | _ -> (outp, ctx)

  let per_node acc tnode =
    match tnode with
    | Block b -> eval_block acc b
    | Scope (b, s) -> eval_scope acc b s

  // Output text, Execution Context
  let initial = "", Map.empty

  let final_output, final_context =
    List.fold per_node initial ast in

  printfn "Rendered Liquid:\n------------\n%s\n------------" final_output

// ast
// |> List.map (fun x -> x |> Tree.syntax_tree_to_string)
// |> String.concat "\n\n\n"
// |> printfn "%s"

// let test () =
//   let alpha_list =
//     List (
//       "abcdefghi"
//       |> Seq.toList
//       |> List.map (fun c -> String (c.ToString ()))
//     )

// let tests =
//   [ "apply_capitalize",
//     [ Value (String "ben");
//       Pipe;
//       Identifier [ "capitalize" ] ];

//     "apply_remove",
//     [ Value (String "hello world");
//       Pipe;
//       Identifier [ "remove" ];
//       Colon;
//       Value (String " ") ];

//     "split by comma",
//     [ Value (String "a, b, c, d");
//       Pipe;
//       Identifier [ "split" ];
//       Colon;
//       Value (String ", ") ];


//     "slice array (1 argument)",
//     [ Value alpha_list;
//       Pipe;
//       Identifier [ "slice" ];
//       Colon;
//       Value (Number 2) ];

//     "slice array (2 arguments)",
//     [ Value alpha_list;
//       Pipe;
//       Identifier [ "slice" ];
//       Colon;
//       Value (Number 1);
//       Comma;
//       Value (Number 4) ] ]

// tests
// |> List.iter (fun (name, tokens) -> printfn "%s:\n  %s" name (tokens |> eval_filter |> liquid_value_to_string))
