module LiquidLexer

open System.Text.RegularExpressions
let test_block = "{% assign alt = year | append: ' Winner' %}"

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
  | Eq
  | Ne
  | Gt
  | Lt
  | Gte
  | Lte
  | Or
  | And
  | Identifier of string
  | Boolean of bool
  | String of string


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
  | Unless -> "Unless"
  | EndUnless -> "EndUnless"
  | Comment -> "Comment"
  | EndComment -> "EndComment"
  | Eq -> "Equals"
  | Ne -> "NotEquals"
  | Gt -> "GreaterThan"
  | Lt -> "LessThan"
  | Gte -> "GreaterThanEquals"
  | Lte -> "LessThanEquals"
  | Or -> "Or"
  | And -> "And"
  | Identifier id -> sprintf "Identifier (%s)" id
  | Boolean b -> sprintf "Boolean (%s)" (if b then "True" else "False")
  | String s -> sprintf "String (%s)" s

let token_to_keyword_string =
  function
  | Assign -> Some "assign"
  | Increment -> Some "increment"
  | Decrement -> Some "decrement"
  | Render -> Some "render"
  | Include -> Some "include"
  | If -> Some "if"
  | Else -> Some "else"
  | EndIf -> Some "endif"
  | For -> Some "for"
  | EndFor -> Some "endfor"
  | Case -> Some "case"
  | EndCase -> Some "endcase"
  | When -> Some "when"
  | Unless -> Some "unless"
  | EndUnless -> Some "endunless"
  | Comment -> Some "comment"
  | EndComment -> Some "endcomment"
  | _ -> None

let keyword_to_token =
  function
  | "assign" -> Some Assign
  | "increment" -> Some Increment
  | "decrement" -> Some Decrement
  | "render" -> Some Render
  | "include" -> Some Include
  | "if" -> Some If
  | "else" -> Some Else
  | "endif" -> Some EndIf
  | "case" -> Some Case
  | "endcase" -> Some EndCase
  | "when" -> Some When
  | "unless" -> Some Unless
  | "endunless" -> Some EndUnless
  | "comment" -> Some Comment
  | "endcomment" -> Some EndComment
  | _ -> None

let lex_keyword (s: string) =
  let keywords =
    [ Assign
      Increment
      Decrement
      Render
      Include
      If
      Else
      EndIf
      For
      EndFor
      Case
      EndCase
      When
      Unless
      EndUnless
      Comment
      EndComment ] in

  let found_keyword =
    (List.tryFind
      (fun k ->
        match token_to_keyword_string k with
        | Some literal -> Regex.IsMatch(s, starts_with_regex literal)
        | _ -> false)
      keywords) in

  match found_keyword with
  | Some fk -> Some fk, s[(token_to_keyword_string fk).Value.Length ..]
  | None -> None, s


let lex_bool (s: string) =
  let literals = [ "true"; "false" ] in

  let found =
    List.tryFind (fun lit -> Regex.IsMatch(s, starts_with_regex lit)) literals in

  match found with
  | Some l -> Some(Boolean(l = "true")), s[l.Length ..]
  | None -> None, s

let lex_string (s: string) =
  let string_regex = "^(\'(?:.+?)(?:[^\\]\')|\"(?:.+?)(?:[^\\]\"))" in

  if Regex.IsMatch(s, string_regex) then
    let m = Regex.Match(s, string_regex) in
    let str_literal = m.Groups.[0].Value in
    let quoteless = str_literal[1 .. str_literal.Length - 1] in
    Some quoteless, s[str_literal.Length ..]
  else
    None, s
