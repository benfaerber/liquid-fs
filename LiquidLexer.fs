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
  | EqEq
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
  | Number of float


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
  | EqEq -> "TestEquality"
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
  | Number n -> sprintf "Number (%f)" n


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
      When, "when"
      Unless, "unless"
      EndUnless, "endunless"
      Comment, "comment"
      EndComment, "endcomment" ] in

  let found_keyword =
    (List.tryFind (fun (_, literal) -> Regex.IsMatch(s, starts_with_regex literal)) keywords) in

  match found_keyword with
  | Some (token, literal) -> Some token, s[literal.Length ..]
  | None -> None, s


let lex_bool (s: string) =
  let literals = [ "true"; "false" ] in

  let found =
    List.tryFind (fun lit -> Regex.IsMatch(s, starts_with_regex lit)) literals in

  match found with
  | Some l -> Some(Boolean(l = "true")), s[l.Length ..]
  | None -> None, s

let lex_string (s: string) =
  let string_regex = "^(\'(?:.+?)(?:[^\\\\]\')|\"(?:.+?)(?:[^\\\\]\"))" in

  if Regex.IsMatch(s, string_regex) then
    let m = Regex.Match(s, string_regex) in
    let str_literal = m.Groups.[0].Value in
    let quoteless = str_literal[1 .. str_literal.Length - 2] in
    Some(String quoteless), s[str_literal.Length ..]
  else
    None, s

(*
| Eq -> "Equals"
| Ne -> "NotEquals"
| Gt -> "GreaterThan"
| Lt -> "LessThan"
| Gte -> "GreaterThanEquals"
| Lte -> "LessThanEquals"
| Or -> "Or"
| And -> "And"
*)

let lex_operator (s: string) =
  let operators =
    [ "==", EqEq
      "=", Eq
      "!=", Ne
      ">=", Gte
      "<=", Lte
      ">", Gt
      "<", Lt
      "or", Or
      "and", And ] in

  let found =
    List.tryFind (fun (literal, _) -> Regex.IsMatch(s, starts_with_regex literal)) operators in

  match found with
  | Some (l, t) -> Some t, s[l.Length ..]
  | None -> None, s

let lex_number (s: string) =
  let number_regex = "((?:-)?(?:\d+)(?:\.)?(?:\d+)?)" in

  if Regex.IsMatch(s, starts_with_regex number_regex) then
    let m = Regex.Match(s, starts_with_regex number_regex) in
    let literal = m.Groups.[0].Value in
    Some(Number((float) literal)), s[literal.Length ..]
  else
    None, s

let lex_token (s: string) =
  let lexers =
    [ lex_keyword
      lex_number
      lex_operator
      lex_string
      lex_bool ] in

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
