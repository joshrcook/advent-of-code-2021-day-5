let testFile = Node.Path.resolve("src/day-6", "test-input.txt")
let realFile = Node.Path.resolve("src/day-6", "input.txt")

let processInput = file =>
  Node.Fs.readFileAsUtf8Sync(file)
  ->Js.String2.split(",")
  ->Js.Array2.map(Belt.Int.fromString)
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)

let tap = (xs, tag) => {
  xs->Js.log2(tag)
  xs
}

let rec spawn = (arr, days) => {
  switch days {
  | 0 => arr
  | _ => spawn(processDay(arr), days - 1)
  }
}
and processDay = arr => {
  let additions = arr->Js.Array2.filter(num => num == 0)->Js.Array2.length->Belt.Array.make(8)
  arr
  ->Js.Array2.map(life =>
    switch life {
    | 0 => 6
    | x => x - 1
    }
  )
  ->Js.Array2.concat(additions)
}

processInput(testFile)->spawn(18)->tap("")->Js.Array2.length->Js.log
