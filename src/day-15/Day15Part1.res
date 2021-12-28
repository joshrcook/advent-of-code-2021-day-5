let _ = {
  let testFile = Node.Path.resolve("src/day-15", "test-input.txt")

  let processInput = filename =>
    filename
    ->Node.Fs.readFileAsUtf8Sync
    ->Js.String2.split("\n")
    ->Belt.Array.map(arr =>
      arr
      ->Js.String2.split("")
      ->Belt.Array.map(val => {
        try {
          val->Belt.Int.fromString->Belt.Option.getExn
        } catch {
        | _ => Js.Exn.raiseError("Input could not be converted to integers")
        }
      })
    )

  //   let graphFromInput = input =>

  try {
    let input = testFile->processInput
    let graph = testFile->processInput->Js.log2("input")
  } catch {
  | Js.Exn.Error(n) => n->Js.log2("error")
  }
}
