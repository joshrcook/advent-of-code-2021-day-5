let testFile = Node.Path.resolve("src/day-7", "test-input.txt")
let realFile = Node.Path.resolve("src/day-7", "input.txt")

let getPositions = filename =>
  Node.Fs.readFileAsUtf8Sync(filename)
  ->Js.String2.split(",")
  ->Js.Array2.map(str => str->Belt.Int.fromString->Belt.Option.getWithDefault(-1))
  ->Js.Array2.filter(n => n != -1)

let rec calcFuelUsage = distance => {
  calcFuelUsageInner(distance, 0)
}
and calcFuelUsageInner = (distance, acc) => {
  switch acc {
  | acc if acc == distance => acc
  | acc => acc + calcFuelUsageInner(distance, acc + 1)
  }
}

let rec getFuelAtPosition = (ls, pos) => {
  switch ls {
  | list{first, ...rest} =>
    (first - pos)->Js.Math.abs_int->calcFuelUsage + getFuelAtPosition(rest, pos)
  | _ => 0
  }
}

realFile
->getPositions
->(ls => {
  let min = ls->Js.Math.minMany_int
  let max = ls->Js.Math.maxMany_int
  let diffs =
    min
    ->Belt.Array.range(max)
    ->Belt.Array.map(pos => ls->Belt.List.fromArray->getFuelAtPosition(pos))
  diffs->Js.log2("diffs")
  diffs->Js.Math.minMany_int
})
->Js.log
