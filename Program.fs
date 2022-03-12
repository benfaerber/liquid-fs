let runner () =
  Liquid.render_file "./liquid/case_test.liquid"
  Liquid.test () |> ignore

runner ()
