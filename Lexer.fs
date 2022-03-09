module Lexer

open System.Text.RegularExpressions

open Syntax

let test_block =
  "{% assign alt = year | append: ' Winner' %}"

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
  | Identifier of string
  | Boolean of bool
  | String of string
  | Number of float
  | Range of int * int

type block_type =
  | Output
  | Statement

type block =
  | Liquid of block_type * token list
  | Text of string

let starts_with_regex txt = "^(" + txt + ")"

let token_to_string =
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
  | Identifier id -> sprintf "Identifier (%s)" id
  | Boolean b -> sprintf "Boolean (%s)" (if b then "True" else "False")
  | String s -> sprintf "String (%s)" s
  | Number n -> sprintf "Number (%f)" n
  | Range (s, e) -> sprintf "Range (%d - %d)" s e

let block_to_string block =
  sprintf
    "Block (%s)"
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

       sprintf "Liquid (%s) - %s" block_name token_string)


let lex_keyword (s: string) =
  let keywords =
    [ Assign, "assign"
      Increment, "increment"
      Decrement, "decrement"
      Render, "render"
      Include, "include"
      If, "if"
      Else, "else"
      EndIf, "endif"
      For, "for"
      EndFor, "endfor"
      Case, "case"
      EndCase, "endcase"
      With, "with"
      When, "when"
      In, "in"
      Unless, "unless"
      EndUnless, "endunless"
      Comment, "comment"
      EndComment, "endcomment"
      EqEq, "=="
      Eq, "="
      Ne, "!="
      Gte, ">="
      Lte, "<="
      Gt, ">"
      Lt, "<"
      Or, "or"
      And, "and"
      Colon, ":"
      Comma, ","
      Empty, "empty"
      Nil, "nil"
      Blank, "blank"
      Break, "break"
      Pipe, "\\|" ] in

  let found_keyword =
    (List.tryFind (fun (_, literal) -> Regex.IsMatch (s, starts_with_regex literal)) keywords) in

  match found_keyword with
  | Some (token, literal) -> Some token, s[literal.Length ..]
  | None -> None, s


let lex_bool (s: string) =
  let literals = [ "true"; "false" ] in

  let found =
    List.tryFind (fun lit -> Regex.IsMatch (s, starts_with_regex lit)) literals in

  match found with
  | Some l -> Some (Boolean (l = "true")), s[l.Length ..]
  | None -> None, s

let match_or_fail s regex modifier =
  let r = starts_with_regex regex in

  if Regex.IsMatch (s, r) then
    let m = Regex.Match (s, r) in
    let literal = m.Groups.[0].Value in
    Some (modifier literal), s[literal.Length ..]
  else
    None, s

let lex_string (s: string) =
  let string_regex =
    "^(\'(?:.+?)(?:[^\\\\]\')|\"(?:.+?)(?:[^\\\\]\"))" in

  match_or_fail s string_regex (fun literal -> String (literal[1 .. literal.Length - 2]))

let lex_number (s: string) =
  let number_regex = "((?:-)?(?:\d+)(?:\.)?(?:\d+)?)" in

  match_or_fail s number_regex (fun literal -> Number (float literal))

let lex_identifier (s: string) =
  let identifier_regex =
    "([A-Za-z_](?:[A-Za-z0-9_\-\.]+)?)" in

  match_or_fail s identifier_regex (fun literal -> Identifier literal)

let lex_range (s: string) =
  let range_regex = "\((\S+)\.\.(\S+)\)" in

  if Regex.IsMatch (s, range_regex) then
    let m = Regex.Match (s, range_regex) in

    match m.Groups |> Seq.toList with
    | literal :: rstart :: rend :: _ -> Some (Range (int rstart.Value, int rend.Value)), s[literal.Length ..]
    | _ -> None, s
  else
    None, s

let consume_whitespace (s: string) =
  let m = Regex.Match (s, starts_with_regex "(\s+)") in
  let g = m.Groups.[0].Value in
  s[g.Length ..]

let lex_token (s: string) =

  let lexers =
    [ lex_keyword
      lex_number
      lex_string
      lex_bool
      lex_range
      lex_identifier ] in

  let found_lexer =
    List.tryFind
      (fun lexer ->
        (match lexer s with
         | Some _, _ -> true
         | _ -> false))
      lexers in

  match found_lexer with
  | Some lexer -> lexer s
  | None -> None, s

let lex_block (s: string) =
  let generator =
    function
    | _, "" -> None
    | Some _, rest ->

      let next_p, next_u =
        rest |> consume_whitespace |> lex_token in

      Some (next_p, (next_p, next_u))
    | _ -> None in

  let initial, rest = lex_token s in

  let optional_ast =
    List.unfold generator (initial, rest) in

  ([ initial ] @ optional_ast)
  |> List.filter (fun opt -> opt.IsSome)
  |> List.map (fun opt -> opt.Value)

let get_tag_innards txt =
  let innards_regex =
    "(?:\{\%|\{\{)-?(?:\s+)?(.+?)(?:\s+)?-?(?:\%\}|\}\})" in

  let m = Regex.Match (txt, innards_regex) in
  m.Groups.[1].Value

let get_block_type txt =
  if Regex.IsMatch (txt, "^(\{\{)") then
    Output
  else
    Statement

let lex_liquid_blocks blocks =
  List.map
    (fun block ->
      if block.IsLiquid then
        let block_type = get_block_type block.Content in

        let tokens =
          block.Content |> get_tag_innards |> lex_block in

        Liquid (block_type, tokens)
      else
        Text block.Content)
    blocks
