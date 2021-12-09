let testFile = Node.Path.resolve("src/day-6", "test-input.txt")
let realFile = Node.Path.resolve("src/day-6", "input.txt")

let number = 1_595_330_616_005

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

// let rec processDays = (eco, days) => {
//   switch days {
//   | 0 => eco
//   | _ => {
//       let updated =
//         eco
//         ->Belt.List.splitAt(1)
//         ->Belt.Option.getExn
//         ->(
//           (first, rest) => {
//             let firstVal = first->Belt.List.get(0)->Belt.Option.getWithDefault(0.)
//             let updatedList =
//               rest
//               ->Belt.List.map(item => {
//                 switch item {
//                 | (idx, val) => (idx - 1, val)
//                 | _ => item
//                 }
//               })
//               //   ->Belt.List.concat(first)
//               ->Belt.List.mapWithIndex((i, x) => {
//                 switch x {
//                 | (6, val) => (6, val +. firstVal)
//                 | _ => x
//                 }
//               })
//           }
//         )
//       //   let firstVal = eco->Belt.Array.slice(~offset=0, ~len=1)
//       //   let restOfArray = eco->Belt.Array.sliceToEnd(1)
//       //   let newEco = restOfArray->Belt.Array.concat(firstVal)
//       //   eco[6] = eco[6] +. new
//       processDays(eco, days - 1)
//     }
//   }
// }

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

processInput(testFile)
->(ls => ls->initEcosystem(Belt.List.makeBy(9, i => (i, 0.))))
->tap("after initial")
// ->processDays(18)
// ->tap("after process")
// ->Js.Array2.reduce((acc, num) => acc +. num, 0.)
->Belt.List.toArray
->Js.log
