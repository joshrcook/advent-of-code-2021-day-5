let testFile = Node.Path.resolve("src/day-12", "test-input.txt")
let realFile = Node.Path.resolve("src/day-12", "input.txt")

let getInput = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n")
  ->Belt.Array.map(str => str->Js.String2.split("-"))
  ->Belt.Array.map(arr =>
    switch arr {
    | [start, end] => Some(start, end)
    | _ => None
    }
  )
  ->Belt.Array.keep(Belt.Option.isSome)
  ->Belt.Array.map(Belt.Option.getExn)
  ->Belt.List.fromArray

let eq = (a, b) => a == b

let input = testFile->getInput

let lists = list{list{"A", "start"}, list{"b", "start"}}

let rec processInput = (lists, input) => {
  switch lists {
  | list{first, ...rest} =>
    switch first {
    | list{head, ..._} =>
      switch head {
      | "end" => list{first, ...processInput(rest, input)}
      | x => {
          let options =
            input
            ->Belt.List.keep(tup => {
              let (start, _) = tup
              x == start
            })
            ->Belt.List.keep(tup => {
              let (_, end) = tup
              switch end->Js.String2.toLowerCase == end {
              | true => first->Belt.List.has(end, eq) == false
              | false => true
              }
            })
            ->Belt.List.map(tup => {
              let (_, end) = tup
              list{end, ...first}
            })
          options->Belt.List.map(Belt.List.toArray)->Belt.List.toArray->Js.log2("options")
          switch options->Belt.List.length {
          | 0 => list{first, ...processInput(rest, input)}
          | _ => options->Belt.List.concat(processInput(rest, input))
          }
        }
      }
    | _ => list{}
    }
  | _ => list{}
  }
}

input->Belt.List.toArray->Js.log
lists->processInput(input)->Belt.List.map(Belt.List.toArray)->Belt.List.toArray->Js.log

// let tap = (ls, tag) => {
//   ls->Js.log2(tag)
//   ls
// }

// testFile
// ->getInput
// // ->tap("after input")
// ->processInput
// // ->tap("after processed")
// // ->Belt.Array.length
// ->Js.log

// // let input = testFile->getInput
// input->Belt.List.toArray->Js.log2("input")

// let processedInput = input->processInput
// processedInput->Js.log2("processed")

// input->processInput->Belt.List.length->Js.log
