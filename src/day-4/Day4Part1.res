let testFile = Node.Path.resolve("src/day-4", "test-input.txt")

let processInput = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n\n")
  ->(
    arr => {
      let numbers =
        arr
        ->Belt.Array.get(0)
        ->Belt.Option.getWithDefault("")
        ->Js.String2.split(",")
        ->Belt.List.fromArray
      let boards =
        arr
        ->Belt.Array.sliceToEnd(1)
        ->Belt.Array.map(board =>
          board
          ->Js.String2.split("\n")
          ->Belt.Array.map(str =>
            str
            ->Js.String2.trim
            ->Js.String2.splitByRe(%re("/\s+/"))
            ->Belt.Array.map(val =>
              switch val {
              | Some(x) => x
              | None => Js.Exn.raiseError("could not split re by whitespace")
              }
            )
            ->Belt.Array.map(Belt.Int.fromString)
            ->Belt.Array.map(val =>
              switch val {
              | Some(x) => x
              | None => Js.Exn.raiseError("could not map strings to numbers")
              }
            )
          )
        )
      (numbers, boards)
    }
  )

testFile
->processInput
->(tuple => {
  let (numbers, boards) = tuple
  boards[0]
})
->Js.log
