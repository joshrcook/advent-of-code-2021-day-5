let testFile = Node.Path.resolve("src/day-4", "test-input.txt")
let realFile = Node.Path.resolve("src/day-4", "input.txt")

let processInput = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n\n")
  ->(
    arr => {
      let numbers =
        arr
        ->Belt.Array.get(0)
        ->Belt.Option.getWithDefault("")
        ->Js.String2.split(",")
        ->Belt.Array.map(Belt.Int.fromString)
        ->Belt.Array.map(val =>
          switch val {
          | Some(x) => x
          | None => Js.Exn.raiseError("strings could not be parsed to ints")
          }
        )
      let boards =
        arr
        ->Belt.Array.sliceToEnd(1)
        ->Belt.Array.map(board =>
          board
          ->Js.String2.split("\n")
          ->Belt.Array.map(str =>
            str
            ->Js.String2.trim
            ->Js.String2.splitByRe(%re("/\s+/"))
            ->Belt.Array.map(val =>
              switch val {
              | Some(x) => x
              | None => Js.Exn.raiseError("could not split re by whitespace")
              }
            )
            ->Belt.Array.map(Belt.Int.fromString)
            ->Belt.Array.map(val =>
              switch val {
              | Some(x) => x
              | None => Js.Exn.raiseError("could not map strings to numbers")
              }
            )
          )
        )
      (numbers, boards)
    }
  )

let didBoardWin = (numbers, board) => {
  let didRowsWin =
    board
    ->Belt.Array.map(row => row->Belt.Array.every(num => numbers->Js.Array2.includes(num)))
    ->Belt.Array.some(val => val == true)
  let didColsWin =
    board
    ->Belt.Array.mapWithIndex((idx, _) =>
      board
      ->Belt.Array.map(row => row[idx])
      ->Belt.Array.every(num => numbers->Js.Array2.includes(num))
    )
    ->Belt.Array.some(val => val == true)
  didRowsWin || didColsWin
}

let rec play = (numbers, boards) => {
  playInternal(numbers, boards, [])
}
and playInternal = (numbers, boards, calledNums) => {
  let firstNumber = numbers->Js.Array2.shift
  switch firstNumber {
  | Some(x) => {
      let _ = calledNums->Js.Array2.push(x)
      let winningBoards = boards->Belt.Array.keep(board => didBoardWin(calledNums, board))
      switch winningBoards->Belt.Array.length {
      | 0 => playInternal(numbers, boards, calledNums)
      | _ => (calledNums, winningBoards)
      }
    }
  | None => (calledNums, [])
  }
}

let getScore = (calledNums, board) => {
  let sum =
    board
    ->Belt.Array.concatMany
    ->Belt.Array.keep(num => calledNums->Js.Array2.includes(num) == false)
    ->Belt.Array.reduce(0, (acc, num) => acc + num)
  let lastNumber = calledNums->Belt.Array.reverse->Belt.Array.get(0)->Belt.Option.getWithDefault(-1)
  sum * lastNumber
}

realFile
->processInput
->(((numbers, boards)) => {
  play(numbers, boards)
})
->(((calledNums, boards)) => {
  let firstBoard = boards->Belt.Array.get(0)->Belt.Option.getWithDefault([[]])
  calledNums->getScore(firstBoard)
})
->Js.log
