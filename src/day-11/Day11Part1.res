open Node
exception InputException
let testFile = Path.resolve("src/day-11", "test-input.txt")

let getInput = file => {
  file
  ->Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n")
  ->Belt.Array.map(str => {
    try {
      str->Js.String2.split("")->Belt.Array.map(str => str->Belt.Int.fromString->Belt.Option.getExn)
    } catch {
    | _ => raise(InputException)
    }
  })
}

let increment = num => num + 1
let incrementMatrix = arr => arr->Belt.Array.map(y => y->Belt.Array.map(increment))

let rec step = (arr, num) => {
  stepInternal(arr, num, 0)
}
and stepInternal = (arr, num, acc) => {
  switch num {
  | 0 => (arr, acc)
  | _ =>
    arr
    ->incrementMatrix
    ->flash(acc)
    ->{
      ((arr, acc)) => arr->stepInternal(num - 1, acc)
    }
  }
}
and flash = (arr, acc) => {
  let newAcc = arr->Belt.Array.reduceWithIndex(acc, (acc, row, y) =>
    acc +
    row->Belt.Array.reduceWithIndex(0, (acc, val, x) => {
      switch val {
      | val if val > 9 => {
          list{
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
            (x - 1, y + 1),
            (x, y + 1),
            (x + 1, y + 1),
          }->Belt.List.forEach(tuple =>
            switch tuple {
            | (x, y) =>
              try {
                let value = arr[y][x]
                switch value {
                | 0 => ()
                | _ => arr[y][x] = arr[y][x] + 1
                }
              } catch {
              | _ => ()
              }
            }
          )
          arr[y][x] = 0
          acc + 1
        }
      | _ => acc
      }
    })
  )
  let flashAgain = arr->Belt.Array.concatMany->Belt.Array.some(val => val > 9)
  switch flashAgain {
  | true => flash(arr, newAcc)
  | false => (arr, newAcc)
  }
}

let matrixToString = arr =>
  arr
  ->Belt.Array.map(arr => arr->Belt.Array.map(Belt.Int.toString)->Js.Array2.joinWith(""))
  ->Js.Array2.joinWith("\n")

let _ = {
  try {
    testFile->getInput->step(0)->Js.log
  } catch {
  | InputException => Js.log("Could not get input")
  | _ => Js.log("An unexpected error occurred")
  }
}
