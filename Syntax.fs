module Syntax

let block_regex =
  "\{\%(?:\-)?(?:\s+)(.+)(?:\s+)(?:-)?\%\}"

let output_regex =
  "\{\{(?:\-)?(?:\s+)?(.+?)(?:\s+)?(?:\-)?\}\}"

let both_regex = block_regex + "|" + output_regex

let starts_with_regex txt = $"^({txt})"
let find_word_regex txt = $"^\\b({txt})\\b"

type simple_block = { Content: string; IsLiquid: bool }

type liquid_value =
  | String of string
  | Boolean of bool
  | Number of float
  | List of liquid_value list
  | NilValue
  | EmptyValue


type token =
  | Assign
  | Increment
  | Decrement
  | Render
  | Include
  | If
  | Else
  | EndIf
  | For
  | EndFor
  | Case
  | EndCase
  | When
  | Unless
  | EndUnless
  | Comment
  | EndComment
  | Capture
  | EndCapture
  | With
  | In
  | Eq
  | EqEq
  | Ne
  | Gt
  | Lt
  | Gte
  | Lte
  | Or
  | And
  | Colon
  | Comma
  | Pipe
  | Blank
  | Empty
  | Nil
  | Break
  | Continue
  | Identifier of string list
  | Value of liquid_value
  | Range of int * int

type block_type =
  | Output
  | Statement

type block =
  | Liquid of block_type * token list
  | Text of string

let rec liquid_value_to_string =
  function
  | Boolean b -> sprintf "Boolean (%s)" (if b then "True" else "False")
  | String s -> sprintf "String (%s)" s
  | Number n -> sprintf "Number (%f)" n
  | NilValue -> sprintf "NilValue"
  | EmptyValue -> sprintf "EmptyValue"
  | List l ->
    sprintf
      "List (%s)"
      (l
       |> List.map liquid_value_to_string
       |> String.concat ", ")


let rec token_to_string =
  function
  | Assign -> "Assign"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Render -> "Render"
  | Include -> "Include"
  | If -> "If"
  | Else -> "Else"
  | EndIf -> "EndIf"
  | For -> "For"
  | EndFor -> "EndFor"
  | Case -> "Case"
  | EndCase -> "EndCase"
  | When -> "When"
  | With -> "With"
  | In -> "In"
  | Unless -> "Unless"
  | EndUnless -> "EndUnless"
  | Comment -> "Comment"
  | EndComment -> "EndComment"
  | Capture -> "Capture"
  | EndCapture -> "EndCapture"
  | Eq -> "Equals"
  | EqEq -> "TestEquality"
  | Ne -> "NotEquals"
  | Gt -> "GreaterThan"
  | Lt -> "LessThan"
  | Gte -> "GreaterThanEquals"
  | Lte -> "LessThanEquals"
  | Or -> "Or"
  | And -> "And"
  | Colon -> "Colon"
  | Comma -> "Comma"
  | Pipe -> "Pipe"
  | Blank -> "Blank"
  | Empty -> "Empty"
  | Nil -> "Nil"
  | Break -> "Break"
  | Continue -> "Continue"
  | Identifier parts -> sprintf "Identifier (%s)" (String.concat "->" parts)
  | Value v -> liquid_value_to_string v
  | Range (s, e) -> sprintf "Range (%d - %d)" s e


let block_to_string block =
  (match block with
   | Text _ -> "Text Block"
   | Liquid (block_type, tokens) ->
     let block_name =
       match block_type with
       | Statement -> "Statement"
       | Output -> "Output" in

     let token_string =
       (tokens
        |> List.map (fun t -> token_to_string t)
        |> String.concat ", ") in

     sprintf "Liquid %s - %s" block_name token_string)

let reverse_tail lst =
  lst |> List.rev |> List.tail |> List.rev

let identifier_to_string id =
  match id with
  | Identifier parts -> String.concat "->" parts
  | _ -> ""

type node =
  | Block of block
  | Scope of block * node list
