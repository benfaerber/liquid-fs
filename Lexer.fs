module Lexer

open System.Text.RegularExpressions

open Syntax


let lex_keyword (s: string) =
  let keywords =
    [ Assign, "assign";
      Increment, "increment";
      Decrement, "decrement";
      Render, "render";
      Include, "include";

      If, "if";
      Else, "else";
      EndIf, "endif";
      For, "for";
      EndFor, "endfor";
      Case, "case";
      EndCase, "endcase";

      With, "with";
      When, "when";
      In, "in";

      Unless, "unless";
      EndUnless, "endunless";
      Comment, "comment";
      EndComment, "endcomment";
      Capture, "capture";
      EndCapture, "endcapture";

      Empty, "empty";
      Nil, "nil";
      Blank, "blank";

      Break, "break";
      Continue, "continue";

      Or, "or";
      And, "and";

      EqEq, "==";
      Eq, "=";
      Ne, "!=";
      Gte, ">=";
      Lte, "<=";
      Gt, ">";
      Lt, "<";

      Colon, ":";
      Comma, ",";
      Pipe, "\\|" ] in

  let letters =
    "abcdefghijklmnopqrstuvwxyz" |> Seq.toList in

  let is_word txt =
    let head = txt |> Seq.toList |> List.head in List.contains head letters

  let get_regex lit =
    lit
    |> if is_word lit then
         find_word_regex
       else
         starts_with_regex

  let found_keyword =
    (List.tryFind (fun (_, literal) -> Regex.IsMatch (s, get_regex literal)) keywords) in

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

let string_regex =
  "^(\'(?:.+?)(?:[^\\\\]\')|\"(?:.+?)(?:[^\\\\]\"))"

let lex_string (s: string) =
  match_or_fail s string_regex (fun literal -> String (literal[1 .. literal.Length - 2]))

let lex_number (s: string) =
  let number_regex = "((?:-)?(?:\d+)(?:\.)?(?:\d+)?)" in

  match_or_fail s number_regex (fun literal -> Number (float literal))

let parse_identifier (id: string) =
  let dotted = id.Split (".") |> Seq.toList in

  let res =
    match dotted with
    | [] -> [ "" ]
    | lst ->
      let last = lst |> List.rev |> List.head in
      let all_but_last = lst |> reverse_tail in
      let bracket_notation = "(.+?)\[(.+)\]" in

      if Regex.IsMatch (last, bracket_notation) then
        let m = Regex.Match (last, bracket_notation) in
        let g = m.Groups in
        let baseid = g.[1].Value in
        let inbracks = g.[2].Value in

        all_but_last @ [ baseid; inbracks ]
      else
        lst in

  Identifier res

let lex_identifier (s: string) =
  let identifier_regex =
    $"([A-Za-z_](?:[A-Za-z0-9_\-\.]+)?)(\[((\d+)|({string_regex}))\])?" in

  match_or_fail s identifier_regex parse_identifier


let lex_range (s: string) =
  let range_regex = "\((\S+)\.\.(\S+)\)" in

  if Regex.IsMatch (s, range_regex) then
    let m = Regex.Match (s, range_regex) in

    match m.Groups |> Seq.toList with
    | literal :: rstart :: rend :: _ ->
      let startend = int rstart.Value, int rend.Value in Some (Range startend), s[literal.Length ..]
    | _ -> None, s
  else
    None, s

let consume_whitespace (s: string) =
  let m = Regex.Match (s, starts_with_regex "(\s+)") in
  let g = m.Groups.[0].Value in
  s[g.Length ..]

let lex_token (s: string) =

  let lexers =
    [ lex_keyword;
      lex_number;
      lex_string;
      lex_bool;
      lex_range;
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

let get_tag_contents txt =
  let innards_regex =
    "(?:\{\%|\{\{)-?(?:\s+)?(.+?)(?:\s+)?-?(?:\%\}|\}\})" in

  let m = Regex.Match (txt, innards_regex) in
  m.Groups.[1].Value

let get_block_type txt =
  if Regex.IsMatch (txt, "^(\{\{)") then
    Output
  else
    Statement

let lex_liquid_blocks =
  List.map
    (fun { Content = content;
           IsLiquid = is_liquid } ->
      if is_liquid then
        let tokens = content |> get_tag_contents |> lex_block in

        Liquid (get_block_type content, tokens)
      else
        Text content)
