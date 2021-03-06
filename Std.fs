module Std

open Syntax
open System.Text.RegularExpressions

let truthy =
  function
  | NilValue
  | Boolean false -> false
  | _ -> true

let falsy v = v |> truthy |> not

let rec abs =
  function
  | Number n when n < 0 -> Number (n * -1.)
  | Number n -> Number n
  | String s -> abs (Number (float s))
  | _ -> Number 0

let append liq_lst liq_val =
  match liq_lst with
  | List lst -> List (lst @ [ liq_val ])
  | _ -> List []

let at_least given_lval min_lval =
  match given_lval, min_lval with
  | Number gv, Number mv -> if gv < mv then Number mv else Number gv
  | _ -> Number 0

let at_most given_lval max_lval =
  match given_lval, max_lval with
  | Number gv, Number mv -> if gv > mv then Number mv else Number gv
  | _ -> Number 0


let capitalize =
  function
  | String "" -> String ""
  | String s ->
    String (
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
  | _ -> String ""


let ceil =
  function
  | Number n -> Number (System.Math.Ceiling n)
  | _ -> Number 0

let compact =
  function
  | List lst ->
    List (
      List.filter
        (function
        | NilValue -> false
        | _ -> true)
        lst
    )
  | _ -> List ([])


let concat curr_li_list add_liq_list =
  match curr_li_list, add_liq_list with
  | List curr_list, List add_list -> List (curr_list @ add_list)
  | _ -> List []

// Skipping Date for now
// I need to decide how I will store liquid dates

// Default is a keyword
let default_value lval ldef =
  match lval, ldef with
  | v, _ when truthy v -> v
  | _, d -> d

let divided_by a b =
  match a, b with
  | Number an, Number bn -> Number (an / bn)
  | _ -> Number 0

let downcase =
  function
  | String s ->
    String (
      s
      |> Seq.toList
      |> List.map System.Char.ToLower
      |> System.String.Concat
    )
  | _ -> String ""

// Skipping escape and escaped once for now

let first =
  function
  | List lst ->
    (match lst with
     | hd :: _ -> hd
     | _ -> NilValue)
  | _ -> NilValue


let floor =
  function
  | Number n -> Number (System.Math.Floor n)
  | _ -> Number 0

let join liq_arr liq_delim =
  match liq_arr, liq_delim with
  | List arr, String delim ->
    String (
      arr
      |> (List.map (function
        | String s -> s
        | _ -> ""))
      |> String.concat delim
    )
  | _ -> NilValue

let last =
  function
  | List lst ->
    (match lst |> List.rev with
     | hd :: _ -> hd
     | _ -> NilValue)
  | _ -> NilValue

let strip_whitespace from_start ls =
  match ls with
  | String s ->
    let reg =
      if from_start then
        "^(\s+)"
      else
        "(\s+)$" in

    String (
      if Regex.IsMatch (s, reg) then
        Regex.Replace (s, reg, "")
      else
        s
    )
  | _ -> NilValue

let lstrip = strip_whitespace true
let rstrip = strip_whitespace false

// Skip map for now, it requires access to var context

let minus a b =
  match a, b with
  | Number an, Number bn -> Number (an - bn)
  | _ -> Number 0

let modulo a b =
  match a, b with
  | Number an, Number bn -> Number (an % bn)
  | _ -> Number 0

let newline_to_br =
  function
  | String s -> String (s.Replace ("\n", "<br />"))
  | _ -> NilValue

let plus a b =
  match a, b with
  | Number an, Number bn -> Number (an + bn)
  | _ -> Number 0

let prepend liq_lst liq_val =
  match liq_lst with
  | List lst -> List ([ liq_val ] @ lst)
  | _ -> List []

let remove lhay lnee =
  match lhay, lnee with
  | String hay, String nee -> String (hay.Replace (nee, ""))
  | _ -> NilValue

// TODO remove_first and replace_first


let replace lhay lnee lrep =
  match lhay, lnee, lrep with
  | String hay, String nee, String rep -> String (hay.Replace (nee, rep))
  | _ -> NilValue

let reverse =
  function
  | List lst -> List (lst |> List.rev)
  | _ -> NilValue

let round =
  function
  | Number n -> Number (System.Math.Round (n))
  | _ -> NilValue

let round_to_places lnumber lplaces =
  match lnumber, lplaces with
  | Number n, Number p -> Number (System.Math.Round (n, int p))
  | _ -> NilValue

let size =
  function
  | List lst -> Number (lst |> List.length |> float)
  | _ -> NilValue

let slice_start llst lstart =
  match llst, lstart with
  | List lst, Number lstart -> List lst[int lstart ..]
  | String str, Number lstart -> String str[int lstart ..]
  | _ -> NilValue

let slice_start_end llst lstart lend =
  match llst, lstart, lend with
  | List lst, Number lstart, Number lend -> List lst[int lstart .. int lend]
  | String str, Number lstart, Number lend -> String str[int lstart .. int lend]
  | _ -> NilValue

// TODO Add support of negative slices

// TODO sort and sort_natural

let split lstr ldelim =
  match lstr, ldelim with
  | String str, String delim -> List (List.map (fun s -> String s) (str.Split (delim) |> Seq.toList))
  | _ -> NilValue

let strip =
  function
  | String _ as s -> s |> lstrip |> rstrip
  | _ -> NilValue

// TODO strip_html strip_newlines

let times a b =
  match a, b with
  | Number an, Number bn -> Number (an * bn)
  | _ -> Number 0


let truncate_custom lstr lchrs lcust =
  match lstr, lchrs, lcust with
  | String s, Number cs, String cust ->
    String (
      if s.Length > int cs then
        s[.. int cs] + cust
      else
        s
    )
  | _ -> NilValue

let truncate lstr lchrs =
  truncate_custom lstr lchrs (String "...")

let truncatewords_custom lstr lwrds lcust =
  match lstr, lwrds, lcust with
  | String s, Number wrds, String cust ->
    let all_words = s.Split (" ") in

    String (
      if all_words.Length > int wrds then
        (all_words[.. int wrds] |> String.concat " ")
        + cust
      else
        s
    )
  | _ -> NilValue

let truncatewords lstr lwrds =
  truncate_custom lstr lwrds (String "...")

// TODO uniq

let upcase =
  function
  | String s ->
    String (
      s
      |> Seq.toList
      |> List.map System.Char.ToUpper
      |> System.String.Concat
    )
  | _ -> String ""

// TODO url_decode url_encode where
