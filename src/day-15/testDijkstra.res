type neighbors = Js.Dict.t<float>
type graph = {start: string, end: string, map: Js.Dict.t<neighbors>}
type node = {
  cost: float,
  parent: option<Js.Dict.key>,
  isProcessed: bool,
}
type nodeState = Js.Dict.t<node>

open Js.Dict

let testFile = Node.Path.resolve("src/day-15", "test-input.txt")
let realFile = Node.Path.resolve("src/day-15", "input.txt")

// let processFileToGraph = filename =>
//   filename
//   ->Node.Fs.readFileAsUtf8Sync
//   ->Js.String2.split("\n")
//   ->Belt.Array.map(str =>
//     str
//     ->Js.String2.split("")
//     ->Belt.Array.map(strNumber => strNumber->Belt.Float.fromString->Belt.Option.getExn)
//   )
//   ->{
//     arr => {
//       let graph = {
//         start: "0,0",
//         end: [
//           (arr->Belt.Array.length - 1)->Belt.Int.toString,
//           (arr->Belt.Array.get(0)->Belt.Option.getWithDefault([])->Belt.Array.length - 1)
//             ->Belt.Int.toString,
//         ]->Js.Array2.joinWith(","),
//         map: arr
//         ->Belt.Array.mapWithIndex((y, row) => row->Belt.Array.mapWithIndex((x, val) => (x, y)))
//         ->Belt.Array.concatMany
//         ->Belt.Array.map(item =>
//           switch item {
//           | (x, y) => ([x, y]->Js.Array2.joinWith(","), [(x + 1, y), (y + 1, x)]->Belt.Array.reduce()
//           }
//         ),
//       }
//     }
//   }

let graph: graph = {
  start: "START",
  end: "FIN",
  map: Js.Dict.fromList(list{
    ("START", Js.Dict.fromList(list{("A", 6.), ("B", 2.)})),
    ("A", Js.Dict.fromList(list{("FIN", 1.)})),
    ("B", Js.Dict.fromList(list{("A", 3.), ("FIN", 5.)})),
    ("FIN", Js.Dict.fromList(list{})),
  }),
}

let graph2 = {
  start: "START",
  end: "FIN",
  map: fromList(list{
    ("START", fromList(list{("A", 5.), ("B", 2.)})),
    ("A", fromList(list{("C", 4.), ("D", 2.)})),
    ("B", fromList(list{("A", 8.), ("D", 7.)})),
    ("C", fromList(list{("FIN", 3.), ("D", 2.)})),
    ("D", fromList(list{("FIN", 1.)})),
    ("FIN", fromList(list{})),
  }),
}

let findLowestCostNode: nodeState => option<(Js.Dict.key, node)> = nodeState =>
  nodeState
  ->Js.Dict.entries
  ->Belt.Array.keep(entry =>
    switch entry {
    | (_, val) => val.isProcessed == false
    }
  )
  ->Belt.Array.reduce(None, (acc, entry) =>
    switch acc {
    | None => Some(entry)
    | Some(_, accNode) =>
      switch entry {
      | (_, entryNode) => entryNode.cost < accNode.cost ? Some(entry) : acc
      }
    }
  )

let getNodeStateFromGraph: graph => nodeState = graph =>
  graph.map
  ->Js.Dict.entries
  ->Belt.Array.reduce([], (acc, entry) =>
    switch entry {
    | (key, _) =>
      acc->Belt.Array.concat([
        (
          key,
          {cost: key == graph.start ? 0. : Pervasives.infinity, isProcessed: false, parent: None},
        ),
      ])
    }
  )
  ->Js.Dict.fromArray

let getNodeNeighbors = (node, graph) =>
  switch node {
  | (key, _) => graph.map->Js.Dict.get(key)->Belt.Option.getWithDefault(Js.Dict.empty())
  }

let updateNodeStateFromNeighbors = (nodeState, neighbors, node) =>
  switch node {
  | (key, theNode) =>
    nodeState->Js.Dict.set(
      key,
      {
        ...theNode,
        isProcessed: true,
      },
    )
    let nodeCost = theNode.cost
    neighbors
    ->Js.Dict.entries
    ->Belt.Array.reduce(nodeState, (acc, neighbor) =>
      switch neighbor {
      | (neighborKey, neighborCost) => {
          let nodeFromState = acc->Js.Dict.get(neighborKey)
          switch nodeFromState {
          | Some(someCurrentNode) => {
              let newCost = neighborCost +. nodeCost
              if newCost < someCurrentNode.cost {
                acc->Js.Dict.set(
                  neighborKey,
                  {
                    ...someCurrentNode,
                    parent: Some(key),
                    cost: newCost,
                  },
                )
              }
              acc
            }
          | None => Js.Exn.raiseError("Could not find node in node state")
          }
        }
      }
    )
  }

let rec dijkstra = graph => {
  let nodeState = graph->getNodeStateFromGraph
  graph->dijkstraInner(nodeState)
}
and dijkstraInner = (graph, nodeState) => {
  let currentNode = nodeState->findLowestCostNode
  currentNode->Js.log2("current node")
  switch currentNode {
  | Some(node) => {
      let neighbors = node->getNodeNeighbors(graph)
      let newNodeState = nodeState->updateNodeStateFromNeighbors(neighbors, node)
      newNodeState->Js.log2("new node state")
      graph->dijkstraInner(newNodeState)
    }
  | None => nodeState
  }
}

let _ = {
  try {
    graph2->Js.log
    graph2->dijkstra->Js.log
  } catch {
  | Js.Exn.Error(_) => "There was a js error"->Js.log
  | _ => "there was an error"->Js.log
  }
}
