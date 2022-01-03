let testFile = Node.Path.resolve("src/day-25", "test-input.txt")

let _ = {
  let processInput = filename =>
    filename
    ->Node.Fs.readFileAsUtf8Sync
    ->Js.String2.split("\n")
    ->Belt.Array.map(str => str->Js.String2.split(""))

  let step = xxs =>
    xxs->Belt.Array.mapWithIndex((y, row) =>
      row->Belt.Array.mapWithIndex((x, val) =>
        switch val {
        | ">" =>
          switch x {
          | x if x + 1 == row->Belt.Array.length => (val, row[0])
          | x => (val, row[x + 1])
          }
        | "v" =>
          switch y {
          | y if y + 1 == xxs->Belt.Array.length => (val, xxs[0][x])
          | y => (val, xxs[y + 1][x])
          }
        | _ => (val, val)
        }
      )
    )

  testFile->processInput->step->Js.log
}
