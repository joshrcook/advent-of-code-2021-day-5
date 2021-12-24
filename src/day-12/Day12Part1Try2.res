let testFile = Node.Path.resolve("src/day-12", "test-input.txt")
let testFile2 = Node.Path.resolve("src/day-12", "test-input2.txt")
let testFile3 = Node.Path.resolve("src/day-12", "test-input3.txt")
let realFile = Node.Path.resolve("src/day-12", "input.txt")

type node = string
type path = array<node>
type paths = array<path>
type connections = array<(node, node)>

let processInput: string => connections = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n")
  ->Belt.Array.map(item => item->Js.String2.split("-"))
  ->Belt.Array.map(arr =>
    switch arr {
    | [first, second] => (first, second)
    | _ => Js.Exn.raiseError("could not get input tuple")
    }
  )
  ->Belt.Array.reduce([], (acc, item) =>
    switch item {
    | ("start", x)
    | (x, "start") =>
      acc->Belt.Array.concat([("start", x)])
    | ("end", x)
    | (x, "end") =>
      acc->Belt.Array.concat([(x, "end")])
    | (x, y) => acc->Belt.Array.concat([(x, y)])->Belt.Array.concat([(y, x)])
    }
  )

let isEnd: string => bool = str => str == "end"

let hasPathEnded: path => bool = path => path->Js.Array2.findIndex(item => item->isEnd) > -1

let nextSteps: (connections, node) => array<node> = (connections, from) => {
  connections
  ->Belt.Array.keep(connection =>
    switch connection {
    | (start, _) => start == from
    }
  )
  ->Belt.Array.map(connection =>
    switch connection {
    | (_, end) => end
    }
  )
}

let isLowerCase: string => bool = str => str == str->Js.String2.toLowerCase

let isStart: string => bool = str => str == "start"

let canPathAccept: (path, node) => bool = (path, next) => {
  switch next {
  | "start" => false
  | x if x->isLowerCase => path->Belt.Array.every(step => step != next)
  | _ => true
  }
}

let tap = (data, label) => {
  data->Js.log2(label)
  data
}

let rec getUniquePaths: connections => paths = connections => {
  iterateInternal([["start"]], connections)
}
and iterateInternal: (paths, connections) => paths = (paths, connections) => {
  switch paths->Belt.Array.every(path => path->hasPathEnded) {
  | true => paths
  | _ => {
      let (ended, unended) = paths->Belt.Array.partition(p => p->hasPathEnded)
      unended
      ->Belt.Array.reduce(ended, (acc, path) => {
        let lastStep = path->Belt.Array.copy->Js.Array2.pop->Belt.Option.getWithDefault("")
        connections
        ->nextSteps(lastStep)
        ->Belt.Array.keep(step => path->canPathAccept(step))
        ->Belt.Array.reduce(acc, (arr, step) => {
          let newPath = path->Belt.Array.concat([step])
          let _ = arr->Js.Array2.push(newPath)
          arr
        })
      })
      ->iterateInternal(connections)
    }
  }
}

let _ = {
  try {
    testFile
    ->processInput
    ->getUniquePaths
    ->tap("final paths")
    ->Belt.Array.length
    ->Js.log2("is the answer")
  } catch {
  | _ => Js.log("something went wrong")
  }
}
