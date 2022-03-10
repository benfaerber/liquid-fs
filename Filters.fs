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
type filter_1 = (liquid_value -> liquid_value)
type filter_2 = (liquid_value -> liquid_value -> liquid_value)
type filter_3 = (liquid_value -> liquid_value -> liquid_value -> liquid_value)

type parameter_count =
  | OneParam
  | TwoParam
  | ThreeParam
  | FourParam

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

let rec abs =
  function
  | LNumber n when n < 0 -> LNumber (n * -1.)
  | LNumber n -> LNumber n
  | LString s -> abs (LNumber (float s))
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

let minus a b =
  match a, b with
  | LNumber an, LNumber bn -> LNumber (an - bn)
  | _ -> LNumber 0

let modulo a b =
  match a, b with
  | LNumber an, LNumber bn -> LNumber (an % bn)
  | _ -> LNumber 0

let newline_to_br =
  function
  | LString s -> LString (s.Replace ("\n", "<br />"))
  | _ -> LNil

let plus a b =
  match a, b with
  | LNumber an, LNumber bn -> LNumber (an + bn)
  | _ -> LNumber 0

let prepend liq_lst liq_val =
  match liq_lst with
  | LList lst -> LList ([ liq_val ] @ lst)
  | _ -> LList []

let remove lhay lnee =
  match lhay, lnee with
  | LString hay, LString nee -> LString (hay.Replace (nee, ""))
  | _ -> LNil

// TODO remove_first and replace_first


let replace lhay lnee lrep =
  match lhay, lnee, lrep with
  | LString hay, LString nee, LString rep -> LString (hay.Replace (nee, rep))
  | _ -> LNil

let reverse =
  function
  | LList lst -> LList (lst |> List.rev)
  | _ -> LNil

let round =
  function
  | LNumber n -> LNumber (System.Math.Round (n))
  | _ -> LNil

let round_to_places lnumber lplaces =
  match lnumber, lplaces with
  | LNumber n, LNumber p -> LNumber (System.Math.Round (n, int p))
  | _ -> LNil

let size =
  function
  | LList lst -> LNumber (lst |> List.length |> float)
  | _ -> LNil

let slice_start llst lstart =
  match llst, lstart with
  | LList lst, LNumber lstart -> LList lst[int lstart ..]
  | _ -> LNil

let slice_start_end llst lstart lend =
  match llst, lstart, lend with
  | LList lst, LNumber lstart, LNumber lend -> LList lst[int lstart .. int lend]
  | _ -> LNil

// TODO Add support of negative slices

// TODO sort and sort_natural

let split lstr ldelim =
  match lstr, ldelim with
  | LString str, LString delim -> LList (List.map (fun s -> LString s) (str.Split (delim) |> Seq.toList))
  | _ -> LNil

let strip =
  function
  | LString _ as s -> s |> lstrip |> rstrip
  | _ -> LNil

// TODO strip_html strip_newlines

let times a b =
  match a, b with
  | LNumber an, LNumber bn -> LNumber (an * bn)
  | _ -> LNumber 0


let truncate_custom lstr lchrs lcust =
  match lstr, lchrs, lcust with
  | LString s, LNumber cs, LString cust ->
    LString (
      if s.Length > int cs then
        s[.. int cs] + cust
      else
        s
    )
  | _ -> LNil

let truncate lstr lchrs =
  truncate_custom lstr lchrs (LString "...")

let truncatewords_custom lstr lwrds lcust =
  match lstr, lwrds, lcust with
  | LString s, LNumber wrds, LString cust ->
    let all_words = s.Split (" ") in

    LString (
      if all_words.Length > int wrds then
        (all_words[.. int wrds] |> String.concat " ")
        + cust
      else
        s
    )
  | _ -> LNil

let truncatewords lstr lwrds =
  truncate_custom lstr lwrds (LString "...")

// TODO uniq

let upcase =
  function
  | LString s ->
    LString (
      s
      |> Seq.toList
      |> List.map System.Char.ToUpper
      |> System.String.Concat
    )
  | _ -> LString ""

// TODO url_decode url_encode where

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


let get =
  function
  | "abs", OneParam -> Single abs
  | "append", TwoParam -> Double append
  | "at_least", TwoParam -> Double at_least
  | "at_most", TwoParam -> Double at_most
  | "capitalize", OneParam -> Single capitalize
  | "ceil", OneParam -> Single ceil
  | "compact", OneParam -> Single compact
  | "concat", TwoParam -> Double concat
  | "default", OneParam -> Double default_value
  | "divided_by", TwoParam -> Double divided_by
  | "downcase", OneParam -> Single downcase
  | "first", OneParam -> Single first
  | "floor", OneParam -> Single floor
  | "join", TwoParam -> Double join
  | "last", OneParam -> Single last
  | "lstrip", OneParam -> Single lstrip
  | "rstrip", OneParam -> Single rstrip
  | "minus", TwoParam -> Double minus
  | "modulo", TwoParam -> Double modulo
  | "newline_to_br", OneParam -> Single newline_to_br
  | "plus", TwoParam -> Double plus
  | "prepend", TwoParam -> Double prepend
  | "remove", TwoParam -> Double remove
  | "replace", ThreeParam -> Triple replace
  | "reverse", OneParam -> Single reverse
  | "round", OneParam -> Single round
  | "round", TwoParam -> Double round_to_places
  | "size", OneParam -> Single size
  | "slice", TwoParam -> Double slice_start
  | "slice", ThreeParam -> Triple slice_start_end
  | "split", TwoParam -> Double split
  | "strip", OneParam -> Single strip
  | "times", TwoParam -> Double times
  | "truncate", TwoParam -> Double truncate
  | "truncate", ThreeParam -> Triple truncate_custom
  | "truncatewords", TwoParam -> Double truncatewords
  | "truncatewords", ThreeParam -> Triple truncatewords_custom
  | "upcase", OneParam -> Single upcase
  | _ -> Single self
