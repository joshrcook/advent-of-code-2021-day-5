let testFile = Node.Path.resolve("src", "test-input.txt")
let realFile = Node.Path.resolve("src", "input.txt")

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
          | [x, y] => Some(x, y)
          | _ => None
          }
        }
      )
    )
    ->Js.Array2.filter(Belt.Option.isSome)
    ->Js.Array2.map(Belt.Option.getExn)
    ->Belt.List.fromArray
  )
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
      xs->Js.Array2.map(_ => ys->Js.Array2.map(_ => 0))
    }
  )
}

let endpoints = getInput(testFile)
let grid = getGridFromEndpoints(endpoints)

grid->Js.log

let duplicatePoints =
  endpoints
  ->Belt.List.keep(ls => {
    switch ls {
    | list{(x1, y1), (x2, y2)} => x1 == x2 || y1 == y2
    | _ => false
    }
  })
  //   ->Belt.List.map(createLineFromEndpoints)
  //   ->Belt.List.keep(Belt.Option.isSome)
  //   ->Belt.List.map(Belt.Option.getExn)
  ->Belt.List.flatten
  ->findDuplicatePoints
duplicatePoints->Belt.List.length->Js.log2("is the answer")
