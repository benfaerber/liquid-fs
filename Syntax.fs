module Syntax

let block_regex = "\{\%(?:\-)?(?:\s+)(.+)(?:\s+)(?:-)?\%\}"
let output_regex = "\{\{(?:\s+)?(.+?)(?:\s+)?\}\}"
let both_regex = block_regex + "|" + output_regex

type block = { Content: string; IsLiquid: bool }
