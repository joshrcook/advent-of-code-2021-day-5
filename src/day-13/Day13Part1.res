let testFile = Node.Path.resolve("src/day-13", "test-input.txt")
let realFile = Node.Path.resolve("src/day-13", "input.txt")
let filename = realFile

let getFileContents = filename => filename->Node.Fs.readFileAsUtf8Sync

let fileContents = filename->getFileContents

let points =
  fileContents
  ->Js.String2.splitByRe(%re("/\\n\\n/"))
  ->Belt.Array.map(x => x->Belt.Option.getWithDefault(""))
  ->Belt.Array.get(0)
  ->Belt.Option.getWithDefault("")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x =>
    x
    ->Js.String2.split(",")
    ->Js.Array2.map(i => i->Belt.Int.fromString->Belt.Option.getWithDefault(-1))
    ->Js.Array2.filter(val => val >= 0)
    ->(
      arr =>
        switch arr {
        | [x, y] => Some(x, y)
        | _ => None
        }
    )
  )
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)

let folds =
  fileContents
  ->Js.String2.split("\n\n")
  ->Belt.Array.get(1)
  ->Belt.Option.getWithDefault("")
  ->Js.String2.replaceByRe(%re("/fold along /g"), "")
  ->Js.String2.split("\n")
  ->Belt.Array.map(x => x->Js.String2.split("="))
  ->Belt.Array.map(arr =>
    switch arr {
    | [char, val] => Some(char, val)
    | _ => None
    }
  )
  ->Belt.Array.keep(Belt.Option.isSome)
  ->Belt.Array.map(Belt.Option.getExn)
  ->Belt.Array.map(tuple =>
    switch tuple {
    | (char, val) => (char, Belt.Int.fromString(val)->Belt.Option.getWithDefault(-1))
    }
  )
  ->Belt.Array.keep(tuple =>
    switch tuple {
    | (_, val) if val == -1 => false
    | _ => true
    }
  )

let xMax =
  points
  ->Belt.Array.map(val =>
    switch val {
    | (x, _) => x
    }
  )
  ->Js.Math.maxMany_int + 1
let yMax =
  points
  ->Belt.Array.map(val =>
    switch val {
    | (_, y) => y
    }
  )
  ->Js.Math.maxMany_int + 1

let matrix = yMax->Belt.Array.make(xMax->Belt.Array.make(0))

let rec addPointsToMatrix = (matrix, points) => {
  switch points {
  | list{first, ...rest} =>
    switch first {
    | (x, y) => {
        let updatedMatrix = matrix->Belt.Array.mapWithIndex((i, arr) =>
          switch i {
          | i if i == y =>
            arr->Belt.Array.mapWithIndex((j, val) =>
              switch j {
              | j if j == x => 1
              | _ => val
              }
            )
          | _ => arr
          }
        )
        addPointsToMatrix(updatedMatrix, rest)
      }
    }
  | _ => matrix
  }
}

let matrixToString = matrix =>
  matrix
  ->Js.Array2.map(arr =>
    arr
    ->Js.Array2.map(i =>
      switch i {
      | 1 => "#"
      | _ => "."
      }
    )
    ->Js.Array2.joinWith("")
  )
  ->Js.Array2.joinWith("\n")

let rec processFolds = (matrix, folds) =>
  switch folds {
  | list{first, ...rest} =>
    switch first {
    | ("x", x) => {
        let newMatrix = matrix->Belt.Array.map(arr => {
          let firstHalf = arr->Belt.Array.slice(~offset=0, ~len=x)
          let secondHalf = arr->Belt.Array.sliceToEnd(x + 1)->Belt.Array.reverse
          firstHalf->Belt.Array.zipBy(secondHalf, (a, b) =>
            switch a + b > 0 {
            | true => 1
            | false => 0
            }
          )
        })
        processFolds(newMatrix, rest)
      }
    | ("y", y) => {
        let firstHalf = matrix->Belt.Array.slice(~offset=0, ~len=y)
        let otherHalf = matrix->Belt.Array.sliceToEnd(y + 1)->Belt.Array.reverse
        let newMatrix = firstHalf->Belt.Array.zipBy(otherHalf, (a, b) =>
          a->Belt.Array.zipBy(b, (a, b) =>
            switch a + b > 0 {
            | true => 1
            | false => 0
            }
          )
        )
        processFolds(newMatrix, rest)
      }
    | _ => processFolds(matrix, rest)
    }
  | _ => matrix
  }

let finalMatrix =
  matrix
  ->addPointsToMatrix(points->Belt.List.fromArray)
  ->processFolds(folds->Belt.List.fromArray->Belt.List.take(1)->Belt.Option.getWithDefault(list{}))

finalMatrix->matrixToString->Js.log

finalMatrix
->Belt.Array.reduce(0, (acc, arr) => acc + arr->Belt.Array.reduce(0, (acc, item) => acc + item))
->Js.log2("is the answer")
