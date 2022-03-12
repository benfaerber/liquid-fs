module Interpreter

open Syntax

type execution_context = Map<string list, liquid_value>

let rec debug_context_to_string ctx =
  let kv_to_string (k, v) =
    let id_str = k |> String.concat "." in
    let value = v |> debug_liquid_value_to_string in
    sprintf "%s = %s" id_str value

  ctx
  |> Map.toList
  |> List.map kv_to_string
  |> String.concat "\n"


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

let lookup_variable (ctx: execution_context) id =
  if ctx.ContainsKey id then
    ctx.[id]
  else
    NilValue

let extract_value (ctx: execution_context) =
  function
  | Identifier id -> Value (lookup_variable ctx id)
  | other -> other

let extract_non_filter_value (ctx: execution_context) =
  function
  | Identifier id, Colon -> Identifier id
  | Identifier id, _ -> extract_value ctx (Identifier id)
  | other, _ -> other

let eval_liquid_expression (outp, ctx: execution_context) tokens =
  match tokens with
  | init_value :: Pipe :: Identifier func_name :: rest ->
    let p_init_value = init_value |> extract_value ctx in
    debug_print_tokens rest

    let final_rest =
      if rest.Length > 0 then
        [ rest |> List.rev |> List.head |> extract_value ctx ]
      else
        [] in

    let fill_params =
      List.mapi (fun index _ -> extract_non_filter_value ctx (rest.[index], rest.[index + 1])) in

    let p_func_params =
      fill_params rest[.. rest.Length - 2] @ final_rest in

    let recon_expr =
      p_init_value
      :: Pipe :: Identifier func_name :: p_func_params in

    recon_expr |> Filter.eval
  | [ Value v ] -> v
  | [ Identifier id ] -> lookup_variable ctx id
  | _ -> NilValue

let output_liquid_value (outp, ctx) v = outp + (v |> value_to_string), ctx

let eval_liquid_statement (outp, ctx: execution_context) tokens =
  match tokens with
  | Assign :: Identifier var_name :: Eq :: tl ->
    let value_to_assign =
      eval_liquid_expression (outp, ctx) tl in outp, ctx.Add ((var_name, value_to_assign))
  | _ -> outp, ctx


let eval_block (outp, ctx) =
  function
  | Text txt -> outp + txt, ctx
  | Liquid (Output, tokens) ->
    eval_liquid_expression (outp, ctx) tokens
    |> output_liquid_value (outp, ctx)
  | Liquid (Statement, tokens) -> eval_liquid_statement (outp, ctx) tokens

let rec eval_scope (outp, ctx) parent children =
  match parent with
  | Liquid (_, tokens) ->
    match tokens with
    | If :: tl -> (outp, ctx)
    | For :: tl -> (outp, ctx)
    | Case :: tl -> (outp, ctx)
    | Unless :: tl -> (outp, ctx)
    | Comment :: tl -> (outp, ctx)
    | Capture :: Identifier id :: tl ->
      let eval_folder acc dnode =
        match dnode with
        | Block bl -> eval_block acc bl
        | Scope (sp, sc) -> eval_scope acc sp sc in

      let cap_outp, cap_ctx =
        children |> List.fold eval_folder (outp, ctx) in

      match id |> List.head with
      | primary_id when primary_id = global_scope_capture -> cap_outp, cap_ctx
      | _ -> outp, ctx.Add (id, String cap_outp)
    | _ -> (outp, ctx)
  | _ -> raise (System.ArgumentException ("A scope must begin with a liquid block"))

let per_node acc =
  function
  | Block block -> eval_block acc block
  | Scope (parent, children) -> eval_scope acc parent children


let rec interpret (ast: node list) =
  // Output text, Execution Context
  let initial =
    "", Map.empty.Add ([ "environment" ], String "Liquid F#")

  let final_output, _ = List.fold per_node initial ast in

  final_output
