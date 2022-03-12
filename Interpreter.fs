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
  | Number n -> sprintf "%d" (int n)
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

let increment_value (outp, ctx: execution_context) id is_increment =
  let inc_id = [ "_increment_variable_" ] @ id in

  if ctx.ContainsKey inc_id then
    match lookup_variable ctx inc_id with
    | Number n ->
      let new_val = n + (if is_increment then 1. else -1.) in

      outp + (sprintf "%f" new_val), ctx.Add (inc_id, Number new_val)
    | _ -> raise (System.ArgumentException ("Cannot increment non numeric value!"))
  else
    let init_val = if is_increment then 0. else -1. in outp + (sprintf "%f" init_val), ctx.Add (inc_id, Number init_val)


let eval_liquid_statement (outp, ctx: execution_context) tokens =
  match tokens with
  | Assign :: Identifier var_name :: Eq :: tl ->
    let value_to_assign =
      eval_liquid_expression (outp, ctx) tl in outp, ctx.Add (var_name, value_to_assign)
  | [ Increment; Identifier id ] -> increment_value (outp, ctx) id true
  | [ Decrement; Identifier id ] -> increment_value (outp, ctx) id false
  | _ -> outp, ctx

let eval_block (outp, ctx) =
  function
  | Text txt -> outp + txt, ctx
  | Liquid (Output, tokens) ->
    eval_liquid_expression (outp, ctx) tokens
    |> output_liquid_value (outp, ctx)
  | Liquid (Statement, tokens) -> eval_liquid_statement (outp, ctx) tokens


let rec eval_condition (outp, ctx) tokens =
  match tokens |> List.map (extract_value ctx) with
  | [ Value a; Operator op; Value b ] -> Operation.eval_operation a op b
  | Value a :: Operator op :: Value b :: And :: tl -> true
  | Value a :: Operator op :: Value b :: Or :: tl -> true
  | Value a :: And :: tl -> true
  | Value a :: Or :: tl -> true
  | _ -> true

let range start_val end_val =
  List.init (end_val - start_val) (fun i -> start_val + i)

let range_to_list start_val end_val =
  List (
    range start_val end_val
    |> List.map (fun v -> Number (int v))
  )

let eval_if eval_next_scope (outp, ctx) children tokens is_flipped =
  debug_print_tokens tokens
  let check = eval_condition (outp, ctx) tokens in
  let pcheck = if is_flipped then not check else check in

  if pcheck then
    let eval_folder acc dnode =
      match dnode with
      | Block bl -> eval_block acc bl
      | Scope (sp, sc) -> eval_next_scope acc sp sc in

    let scope_outp, scope_cxt =
      children |> List.fold eval_folder (outp, ctx) in

    (scope_outp, scope_cxt)
  else
    (outp, ctx)

let eval_forloop eval_next_scope (outp, ctx) children item_name collection =
  match collection |> extract_value ctx with
  | Value (List lst) ->
    let for_loop_iteration (ioutp, ictx: execution_context) item =
      printfn "%s" (item |> value_to_string)

      let eval_folder acc dnode =
        match dnode with
        | Block bl -> eval_block acc bl
        | Scope (sp, sc) -> eval_next_scope acc sp sc in

      let cap_outp, _ =
        children
        |> List.fold eval_folder (ioutp, ictx.Add (item_name, item)) in

      cap_outp, ctx in

    List.fold for_loop_iteration (outp, ctx) lst
  | _ -> raise (System.ArgumentException ("Cannot loop over non list value!"))

let eval_capture eval_next_scope (outp, ctx) children id =
  let eval_folder acc dnode =
    match dnode with
    | Block bl -> eval_block acc bl
    | Scope (sp, sc) -> eval_next_scope acc sp sc in

  let cap_outp, cap_ctx =
    children |> List.fold eval_folder (outp, ctx) in

  match id |> List.head with
  | primary_id when primary_id = global_scope_capture -> cap_outp, cap_ctx
  | _ -> "", ctx.Add (id, String cap_outp)


let rec eval_scope (outp, ctx: execution_context) parent children =
  match parent with
  | Liquid (_, tokens) ->
    match tokens with
    | If :: tl -> eval_if eval_scope (outp, ctx) children tl false
    | Unless :: tl -> eval_if eval_scope (outp, ctx) children tl true
    | [ For; Identifier item_name; In; Range (rs, re) ] ->
      eval_forloop eval_scope (outp, ctx) children item_name (Value (range_to_list rs re))
    | [ For; Identifier item_name; In; collection ] -> eval_forloop eval_scope (outp, ctx) children item_name collection
    | Case :: tl -> (outp, ctx)

    | Comment :: tl -> (outp, ctx)
    | Capture :: Identifier id :: _ -> eval_capture eval_scope (outp, ctx) children id

    | _ -> (outp, ctx)
  | _ -> raise (System.ArgumentException ("A scope must begin with a liquid block"))


let rec interpret (ast: node list) =
  let per_node acc =
    function
    | Block block -> eval_block acc block
    | Scope (parent, children) -> eval_scope acc parent children

  // Output text, Execution Context
  let initial =
    "", Map.empty.Add ([ "environment" ], String "Liquid F#")

  let final_output, _ = List.fold per_node initial ast in

  final_output
