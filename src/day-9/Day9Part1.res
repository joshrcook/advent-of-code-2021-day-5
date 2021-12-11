let testFile = Node.Path.resolve("src/day-9", "test-input.txt")
let realFile = Node.Path.resolve("src/day-9", "input.txt")

let getArrayFromFile = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Belt.Array.map(str =>
    str
    ->Belt.Option.getWithDefault("")
    ->Js.String2.split("")
    ->Js.Array2.map(str => str->Belt.Int.fromString->Belt.Option.getWithDefault(-1))
    ->Js.Array2.filter(n => n != -1)
  )

let getMatrixValue = (arr, i, j) =>
  arr->Belt.Array.get(i)->Belt.Option.getWithDefault([])->Belt.Array.get(j)

let getAdjacentMatrixValues = (arr, i, j) => {
  [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
  ->Js.Array2.map(tup =>
    switch tup {
    | (x, y) => arr->getMatrixValue(x, y)
    }
  )
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
}

realFile
->getArrayFromFile
// get value with low point boolean
->(arr => {
  arr->Js.Array2.mapi((row, i) =>
    row->Js.Array2.mapi((value, j) => {
      let adjacentValues = arr->getAdjacentMatrixValues(i, j)
      let lowestAdjacent = adjacentValues->Js.Math.minMany_int
      let isLowPoint = value < lowestAdjacent
      (value, isLowPoint)
    })
  )
})
->// flatten matrix
Belt.Array.concatMany
// keep low points
->Belt.Array.keep(value =>
  switch value {
  | (_, true) => true
  | _ => false
  }
)
// get risk value
->Belt.Array.map(val =>
  switch val {
  | (val, _) => val + 1
  }
)
// reduce to sum
->Belt.Array.reduce(0, (acc, item) => acc + item)
->Js.log2("is the answer")
