let testFile = Node.Path.resolve("src/day-8", "test-input.txt")
let realFile = Node.Path.resolve("src/day-8", "input.txt")

let processFile = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
  ->Js.Array2.map(str => str->Js.String2.split(" | "))
//   ->Js.Array2.map(Js.String2.split("|"))

realFile
->processFile
->Js.Array2.map(arr => arr->Belt.Array.get(1))
->Js.Array2.filter(Belt.Option.isSome)
->Js.Array2.map(Belt.Option.getExn)
->Js.Array2.map(str => str->Js.String2.split(" ")->Js.Array2.map(Js.String2.length))
->Belt.Array.concatMany
->Js.Array2.filter(num =>
  switch num {
  | 2
  | 3
  | 4
  | 7 => true
  | _ => false
  }
)
->Js.Array2.length
->Js.log
