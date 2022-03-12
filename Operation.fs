module Operation

open Syntax

let is_truthy =
  function
  | NilValue
  | Boolean false -> false
  | _ -> true

let is_eq liquid_a liquid_b =
  match liquid_a, liquid_b with
  | Number a, Number b -> a = b
  | String a, String b -> a = b
  | Boolean a, Boolean b -> a = b
  | List a, List b ->
    a.Length = b.Length
    && (List.map2 (fun v1 v2 -> v1 = v2) a b
        |> List.contains false
        |> not)
  | _ -> false

let is_ne a b = is_eq a b |> not

let is_gt liquid_a liquid_b =
  match liquid_a, liquid_b with
  | Number a, Number b -> a > b
  | List a, List b -> a.Length > b.Length
  | _ -> false

let is_lt liquid_a liquid_b =
  match liquid_a, liquid_b with
  | Number a, Number b -> a < b
  | List a, List b -> a.Length < b.Length
  | _ -> false

let is_gte a b = is_gt a b || is_eq a b
let is_lte a b = is_lt a b || is_eq a b

let contains_operator liquid_a liquid_b =
  match liquid_a, liquid_b with
  | List a, String b -> List.contains (String b) a
  | List a, Boolean b -> List.contains (Boolean b) a
  | List a, Number b -> List.contains (Number b) a
  | List a, NilValue -> List.contains (NilValue) a
  | List a, EmptyValue -> List.contains (EmptyValue) a
  | String a, String b -> a.Contains (b)
  | _ -> false


let eval_operation a op b =
  let type_check =
    match a, b with
    | String _, String _
    | Boolean _, Boolean _
    | Number _, Number _
    | List _, List _
    | NilValue, NilValue
    | EmptyValue, EmptyValue -> true
    | _ -> false in

  if type_check then
    match op with
    | EqEq -> is_eq a b
    | Ne -> is_ne a b
    | Gt -> is_gt a b
    | Lt -> is_lt a b
    | Gte -> is_gte a b
    | Lte -> is_lte a b
    | Contains -> contains_operator a b
  else
    false
