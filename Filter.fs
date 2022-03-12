module Filter

open Syntax
open System.Text.RegularExpressions

// Referring to the number of parameters a function was provided
type parameter_count =
  | OneParam
  | TwoParam
  | ThreeParam
  | FourParam

// Referring to the number of arguments a function accepts
type liquid_filter =
  | OneArgument of (liquid_value -> liquid_value)
  | TwoArgument of (liquid_value -> liquid_value -> liquid_value)
  | ThreeArgument of (liquid_value -> liquid_value -> liquid_value -> liquid_value)
  | FourArgument of (liquid_value -> liquid_value -> liquid_value -> liquid_value -> liquid_value)


let get_param_count =
  function
  | 1 -> OneParam
  | 2 -> TwoParam
  | 3 -> ThreeParam
  | 4 -> FourParam
  | _ -> raise (System.ArgumentException ("Invalid param count!"))

let param_count_to_int =
  function
  | OneParam -> 1
  | TwoParam -> 2
  | ThreeParam -> 3
  | FourParam -> 4


let lookup_filter =
  function
  | "abs", OneParam -> OneArgument Std.abs
  | "append", TwoParam -> TwoArgument Std.append
  | "at_least", TwoParam -> TwoArgument Std.at_least
  | "at_most", TwoParam -> TwoArgument Std.at_most
  | "capitalize", OneParam -> OneArgument Std.capitalize
  | "ceil", OneParam -> OneArgument Std.ceil
  | "compact", OneParam -> OneArgument Std.compact
  | "concat", TwoParam -> TwoArgument Std.concat
  | "default", OneParam -> TwoArgument Std.default_value
  | "divided_by", TwoParam -> TwoArgument Std.divided_by
  | "downcase", OneParam -> OneArgument Std.downcase
  | "first", OneParam -> OneArgument Std.first
  | "floor", OneParam -> OneArgument Std.floor
  | "join", TwoParam -> TwoArgument Std.join
  | "last", OneParam -> OneArgument Std.last
  | "lstrip", OneParam -> OneArgument Std.lstrip
  | "rstrip", OneParam -> OneArgument Std.rstrip
  | "minus", TwoParam -> TwoArgument Std.minus
  | "modulo", TwoParam -> TwoArgument Std.modulo
  | "newline_to_br", OneParam -> OneArgument Std.newline_to_br
  | "plus", TwoParam -> TwoArgument Std.plus
  | "prepend", TwoParam -> TwoArgument Std.prepend
  | "remove", TwoParam -> TwoArgument Std.remove
  | "replace", ThreeParam -> ThreeArgument Std.replace
  | "reverse", OneParam -> OneArgument Std.reverse
  | "round", OneParam -> OneArgument Std.round
  | "round", TwoParam -> TwoArgument Std.round_to_places
  | "size", OneParam -> OneArgument Std.size
  | "slice", TwoParam -> TwoArgument Std.slice_start
  | "slice", ThreeParam -> ThreeArgument Std.slice_start_end
  | "split", TwoParam -> TwoArgument Std.split
  | "strip", OneParam -> OneArgument Std.strip
  | "times", TwoParam -> TwoArgument Std.times
  | "truncate", TwoParam -> TwoArgument Std.truncate
  | "truncate", ThreeParam -> ThreeArgument Std.truncate_custom
  | "truncatewords", TwoParam -> TwoArgument Std.truncatewords
  | "truncatewords", ThreeParam -> ThreeArgument Std.truncatewords_custom
  | "upcase", OneParam -> OneArgument Std.upcase
  | fname, pcount ->
    raise (
      System.ArgumentException (
        sprintf "Function \"%s\" with %d parameters does not exists!" fname (pcount |> param_count_to_int)
      )
    )

let get filter_id param_count =
  lookup_filter (filter_id |> List.head, get_param_count param_count)

let raise_arg_error count =
  raise (System.ArgumentException (sprintf "Expected a function with %d arguments!" count))

let rec eval =
  function
  | [ Value v; Pipe; Identifier func_name; Colon; Value p1; Comma; Value p2 ] ->
    let filter =
      match get func_name 3 with
      | ThreeArgument f -> f
      | _ -> raise_arg_error 3 in

    filter v p1 p2
  | Value v :: Pipe :: Identifier func_name :: Colon :: Value p1 :: Comma :: Value p2 :: tl ->
    let filter =
      match get func_name 3 with
      | ThreeArgument f -> f
      | _ -> raise_arg_error 3 in

    printfn "F3:"
    debug_print_tokens tl
    eval ([ Value (filter v p1 p2) ] @ tl)

  | [ Value v; Pipe; Identifier func_name; Colon; Value p1 ] ->
    let filter =
      match get func_name 2 with
      | TwoArgument f -> f
      | _ -> raise_arg_error 2 in

    filter v p1
  | Value v :: Pipe :: Identifier func_name :: Colon :: Value p1 :: tl ->
    let filter =
      match get func_name 2 with
      | TwoArgument f -> f
      | _ -> raise_arg_error 2 in

    printfn "F2:"
    debug_print_tokens ([ Value (filter v p1) ] @ tl)
    eval ([ Value (filter v p1) ] @ tl)

  | [ Value v; Pipe; Identifier func_name ] ->
    let filter =
      match get func_name 1 with
      | OneArgument f -> f
      | _ -> raise_arg_error 1 in

    filter v
  | Value v :: Pipe :: Identifier func_name :: tl ->
    let filter =
      match get func_name 1 with
      | OneArgument f -> f
      | _ -> raise_arg_error 1 in

    printfn "F1:"
    debug_print_tokens ([ Value (filter v) ] @ tl)
    eval ([ Value (filter v) ] @ tl)

  | [ Value v ] -> v
  | other ->
    printfn "Other Filter:"
    debug_print_tokens other
    NilValue
