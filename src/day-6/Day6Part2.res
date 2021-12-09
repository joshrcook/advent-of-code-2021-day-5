let testFile = Node.Path.resolve("src/day-6", "test-input.txt")
let realFile = Node.Path.resolve("src/day-6", "input.txt")

let processInput = file =>
  Node.Fs.readFileAsUtf8Sync(file)
  ->Js.String2.split(",")
  ->Js.Array2.map(Belt.Int.fromString)
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
  | x => {
      let zeroes = eco[0]
      eco->Belt.Array.forEachWithIndex((item, idx) => {
        switch idx {
        | 0 => eco[idx] = 0
        | 8 => {
            eco[idx] = zeroes
            eco[6] = eco[6] + zeroes
          }
        | x => eco[idx - 1] = x
        }
      })
      processDays(eco, days - 1)
    }
  }
}

let rec initEcosystem = (ls, eco) =>
  switch ls {
  | list{first, ...rest} =>
    switch first {
    | x => {
        eco[x] = eco[x] + 1
        initEcosystem(rest, eco)
      }
    }
  | _ => eco
  }

processInput(testFile)
->(ls => {
  let initialEcosystem = Belt.Array.make(9, 0)
  ls->initEcosystem(initialEcosystem)
})
->tap("after initial")
->Js.Array2.reduce((acc, num) => acc + num, 0)
->Js.log
