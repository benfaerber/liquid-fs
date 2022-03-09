module Interpreter

open Syntax

let rec interpret_statement =
  function
  | Assign :: Identifier name :: Eq :: tl -> sprintf "Setting variable %s to value: %s" name (interpret_statement tl)
  | [ Render; String filename ] -> sprintf "Rendering %s" filename
  | Render :: String filename :: With :: tl -> sprintf "Rendering %s with variables" filename
  | [ Number n ] -> sprintf "%f" n
  | [ Boolean b ] -> sprintf "%b" b
  | [ String s ] -> sprintf "%s" s
  | _ -> sprintf "Other"
