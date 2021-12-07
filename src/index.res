type line = {
  x1: int,
  x2: int,
  y1: int,
  y2: int,
}

let testFile = Node.Path.resolve("src", "test-input.txt")
let realFile = Node.Path.resolve("src", "input.txt")

let getInput = file =>
  file
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Js.Array2.filter(Js.Option.isSome)
  ->Js.Array2.map(Js.Option.getExn)
  ->Js.Array2.map(str =>
    str
    ->Js.String2.split(" -> ")
    ->Js.Array2.map(x => x->Js.String2.split(","))
    ->Js.Array.concatMany([])
    ->Js.Array2.map(Belt.Int.fromString)
    ->Js.Array2.filter(Js.Option.isSome)
    ->Js.Array2.map(Js.Option.getExn)
  )
  ->Js.Array2.map(x => {
    switch x {
    | [x1, x2, y1, y2] => Some({x1: x1, x2: x2, y1: y1, y2: y2})
    | _ => None
    }
  })
  ->Js.Array2.filter(Js.Option.isSome)
  ->Js.Array2.map(Js.Option.getExn)
  ->Belt.List.fromArray

let input = getInput(testFile)
Js.log(input)
// ->Js.Array2.map(Belt.Int.fromString)
