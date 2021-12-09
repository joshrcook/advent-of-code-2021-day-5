let testFile = Node.Path.resolve("src", "test-input.txt")
let realFile = Node.Path.resolve("src", "input.txt")

type point = {
  x: int,
  y: int,
}

type line = {
  start: point,
  end: point,
}

let getInput = file =>
  file
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Js.Array2.filter(Js.Option.isSome)
  ->Js.Array2.map(Js.Option.getExn)
  ->Js.Array2.map(str =>
    str
    ->Js.String2.split(" -> ")
    ->Js.Array2.map(x =>
      x
      ->Js.String2.split(",")
      ->Js.Array2.map(Belt.Int.fromString)
      ->Js.Array2.filter(Belt.Option.isSome)
      ->Js.Array2.map(Belt.Option.getExn)
      ->(
        xy => {
          switch xy {
          | [x, y] => Some({x: x, y: y})
          | _ => None
          }
        }
      )
    )
    ->Js.Array2.filter(Belt.Option.isSome)
    ->Js.Array2.map(Belt.Option.getExn)
    ->Belt.List.fromArray
  )
  ->Js.Array2.map(ls => {
    switch ls {
    | list{firstPoint, secondPoint} =>
      Some({
        start: firstPoint,
        end: secondPoint,
      })
    | _ => None
    }
  })
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
  ->Belt.List.fromArray

let getRange = (point1, point2) => {
  if point1 <= point2 {
    Belt.Array.range(point1, point2)->Belt.List.fromArray
  } else {
    Belt.Array.range(point2, point1)->Belt.List.fromArray->Belt.List.reverse
  }
}

let rec duplicateHeadToMatchLen = (ls, len) => {
  switch len - ls->Belt.List.length {
  | x if x > 0 => duplicateHeadToMatchLen(list{List.hd(ls), ...ls}, len)
  | _ => ls
  }
}

let tap = (ls, tag) => {
  ls->Js.log2(tag)
  ls
}

let rec findDuplicatePoints = ls => {
  switch ls {
  | list{first, ...rest} => {
      let has = rest->Belt.List.has(first, (a, b) => a == b)
      switch has {
      | true => list{first, ...findDuplicatePoints(rest->Belt.List.keep(x => x != first))}
      | false => findDuplicatePoints(rest)
      }
    }
  | _ => list{}
  }
}

let getGridFromEndpoints = ls => {
  ls
  ->Belt.List.map(line => {
    let {start, end} = line
    let {x: x1, y: y1} = start
    let {x: x2, y: y2} = end
    list{(x1, y1), (x2, y2)}
  })
  ->Belt.List.flatten
  ->Belt.List.reduce((0, 0), ((acc_x, acc_y), (x, y)) => (
    acc_x->Js.Math.max_int(x),
    acc_y->Js.Math.max_int(y),
  ))
  ->(
    tup => {
      let (x, y) = tup
      let xs = Belt.Array.range(0, x)
      let ys = Belt.Array.range(0, y)
      ys->Js.Array2.map(_ => xs->Js.Array2.map(_ => 0))
    }
  )
}

let endpoints = getInput(testFile)

let int_compare = (int1, int2) => {
  switch int2 - int1 {
  | x if x < 0 => -1
  | x if x > 0 => 1
  | 0
  | _ => 0
  }
}

let createDiagramFromGrid = grid => {
  grid
  ->Js.Array2.reverseInPlace
  ->Js.Array2.map(line =>
    line
    ->Js.Array2.map(number => number->Belt.Int.toString)
    ->Js.Array2.map(str =>
      switch str {
      | "0" => "."
      | _ => str
      }
    )
    ->Js.Array2.joinWith("")
  )
  ->Js.Array2.joinWith("\n")
}

endpoints
->Belt.List.keep(({start, end}) => {
  let {x: x1, y: y1} = start
  let {x: x2, y: y2} = end
  x1 == x2 || y1 == y2
})
->(ls => {
  let grid = getGridFromEndpoints(endpoints)
  let rec modifyGrid = ({start, end}) => {
    let {x: x1, y: y1} = start
    let {x: x2, y: y2} = end
    grid[y1][x1] = grid[y1][x1] + 1
    if x1 != x2 || y1 != y2 {
      modifyGrid({start: {x: x1 + int_compare(x1, x2), y: y1 + int_compare(y1, y2)}, end: end})
    }
  }
  ls->Belt.List.forEach(line => {
    modifyGrid(line)
  })
  grid->createDiagramFromGrid->Js.log
  grid
})
->Js.Array2.map(arr => arr->Js.Array2.filter(i => i > 1)->Js.Array2.length)
->Js.Array2.reduce((acc, i) => acc + i, 0)
->Js.log
