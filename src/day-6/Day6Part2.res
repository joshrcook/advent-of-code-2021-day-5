let testFile = Node.Path.resolve("src/day-6", "test-input.txt")
let realFile = Node.Path.resolve("src/day-6", "input.txt")

let processInput = file =>
  Node.Fs.readFileAsUtf8Sync(file)
  ->Js.String2.split(",")
  ->Js.Array2.map(Belt.Float.fromString)
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
  ->Belt.List.fromArray

let tap = (xs, tag) => {
  xs->Js.log2(tag)
  xs
}

let rec processDays = (eco, days) => {
  switch days {
  | 0 => eco
  | _ => {
      let first = eco->Belt.List.take(1)->Belt.Option.getWithDefault(list{})
      let firstVal = first->Belt.List.getAssoc(0, (a, b) => a == b)->Belt.Option.getWithDefault(0.)
      let newList =
        eco
        ->Belt.List.drop(1)
        ->Belt.Option.getWithDefault(list{})
        ->Belt.List.concat(first)
        ->Belt.List.mapWithIndex((i, x) => {
          let (_, val) = x
          switch i {
          | 6 => (6, val +. firstVal)
          | i => (i, val)
          }
        })
      processDays(newList, days - 1)
    }
  }
}

let rec initEcosystem = (ls, eco) => {
  switch ls {
  | list{first, ...rest} => {
      let idx = first->Belt.Int.fromFloat
      let prevVal = eco->Belt.List.getAssoc(idx, (a, b) => a == b)->Belt.Option.getWithDefault(0.)
      initEcosystem(rest, eco->Belt.List.setAssoc(idx, prevVal +. 1., (a, b) => a == b))
    }
  | _ => eco
  }
}

processInput(realFile)
->(ls => ls->initEcosystem(Belt.List.makeBy(9, i => (i, 0.))))
->tap("after initial")
->processDays(256)
->(ls => {
  ls->Belt.List.toArray->Js.log2("after process")
  ls
})
->Belt.List.reduce(0., (acc, pair) =>
  switch pair {
  | (_, val) => acc +. val
  }
)
->Js.log
