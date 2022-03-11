module Filter

open Syntax
open System.Text.RegularExpressions

(*
  How params are passed:
  self
  self param1 param2

  Examples

  {{ -17 | abs }}
  abs = self -> liquid_value
  {{ "/my/fancy/url" | append: ".html" }}
  append = self -> param1 -> liquid_value
*)

// Refering to the number of parameters the function accepts

type parameter_count =
  | OneParam
  | TwoParam
  | ThreeParam
  | FourParam

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



let nyi =
  function
  | s ->
    printfn "Not yet implemented!"
    s

let odne =
  function
  | s ->
    printfn "Overload does not exist!"
    s


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
  | _ -> raise (System.ArgumentException ("Function does not exists!"))

let get filter_id param_count =
  lookup_filter (filter_id |> List.head, get_param_count param_count)
