let runner () =
  Liquid.render_file "./liquid/forloop_test.liquid"
  Liquid.test () |> ignore

runner ()
