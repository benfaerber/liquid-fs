module Filters

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
type liquid_filter =
  | Single of (liquid_value -> liquid_value)
  | Double of (liquid_value -> liquid_value -> liquid_value)
  | Triple of (liquid_value -> liquid_value -> liquid_value -> liquid_value)

let truthy =
  function
  | LNil
  | LBoolean false -> false
  | _ -> true

let falsy v = v |> truthy |> not

let self v = v

let abs =
  function
  | LNumber n when n < 0 -> LNumber (n * -1.)
  | LNumber n -> LNumber n
  | _ -> LNumber 0

let append liq_lst liq_val =
  match liq_lst with
  | LList lst -> LList (lst @ [ liq_val ])
  | _ -> LList []

let at_least given_lval min_lval =
  match given_lval, min_lval with
  | LNumber gv, LNumber mv ->
    if gv < mv then
      LNumber mv
    else
      LNumber gv
  | _ -> LNumber 0

let at_most given_lval max_lval =
  match given_lval, max_lval with
  | LNumber gv, LNumber mv ->
    if gv > mv then
      LNumber mv
    else
      LNumber gv
  | _ -> LNumber 0


let capitalize =
  function
  | LString "" -> LString ""
  | LString s ->
    LString (
      (s
       |> Seq.toList
       |> List.head
       |> System.Char.ToUpper)
        .ToString ()
      + (s
         |> Seq.toList
         |> List.tail
         |> System.String.Concat)
    )
  | _ -> LString ""


let ceil =
  function
  | LNumber n -> LNumber (System.Math.Ceiling n)
  | _ -> LNumber 0

let compact =
  function
  | LList lst ->
    LList (
      List.filter
        (function
        | LNil -> false
        | _ -> true)
        lst
    )
  | _ -> LList ([])


let concat curr_li_list add_liq_list =
  match curr_li_list, add_liq_list with
  | LList curr_list, LList add_list -> LList (curr_list @ add_list)
  | _ -> LList []

// Skipping Date for now
// I need to decide how I will store liquid dates

// Default is a keyword
let default_value lval ldef =
  match lval, ldef with
  | v, _ when truthy v -> v
  | _, d -> d

let divided_by a b =
  match a, b with
  | LNumber an, LNumber bn -> LNumber (an / bn)
  | _ -> LNumber 0

let downcase =
  function
  | LString s ->
    LString (
      s
      |> Seq.toList
      |> List.map System.Char.ToLower
      |> System.String.Concat
    )
  | _ -> LString ""

// Skipping escape and escaped once for now

let first =
  function
  | LList lst ->
    (match lst with
     | hd :: _ -> hd
     | _ -> LNil)
  | _ -> LNil


let floor =
  function
  | LNumber n -> LNumber (System.Math.Floor n)
  | _ -> LNumber 0

let join liq_arr liq_delim =
  match liq_arr, liq_delim with
  | LList arr, LString delim ->
    LString (
      arr
      |> (List.map (function
        | LString s -> s
        | _ -> ""))
      |> String.concat delim
    )
  | _ -> LNil

let last =
  function
  | LList lst ->
    (match lst |> List.rev with
     | hd :: _ -> hd
     | _ -> LNil)
  | _ -> LNil

let strip_whitespace from_start ls =
  match ls with
  | LString s ->
    let reg =
      if from_start then
        "^(\s+)"
      else
        "(\s+)$" in

    LString (
      if Regex.IsMatch (s, reg) then
        Regex.Replace (s, reg, "")
      else
        s
    )
  | _ -> LNil

let lstrip = strip_whitespace true
let rstrip = strip_whitespace false

// Skip map for now, it requires access to var context

let get =
  function
  | "abs" -> Single abs
  | "append" -> Double append
  | "at_least" -> Double at_least
  | "at_most" -> Double at_most
  | "capitalize" -> Single capitalize
  | "ceil" -> Single ceil
  | "compact" -> Single compact
  | "concat" -> Double concat
  | "default" -> Double default_value
  | _ -> Single self
