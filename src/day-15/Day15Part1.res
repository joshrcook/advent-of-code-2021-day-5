let _ = {
  let testFile = Node.Path.resolve("src/day-15", "test-input.txt")

  let xyToKey: (int, int) => string = (x, y) =>
    [x, y]->Belt.Array.map(Belt.Int.toString)->Js.Array2.joinWith(",")

  let processInput = filename =>
    filename
    ->Node.Fs.readFileAsUtf8Sync
    ->Js.String2.split("\n")
    ->Belt.Array.map(arr =>
      arr
      ->Js.String2.split("")
      ->Belt.Array.map(val => {
        try {
          val->Belt.Float.fromString->Belt.Option.getExn
        } catch {
        | _ => Js.Exn.raiseError("Input could not be converted to integers")
        }
      })
    )

  let inputToGraph = xxs => {
    xxs
    ->Belt.Array.mapWithIndex((y, row) =>
      row->Belt.Array.mapWithIndex((x, _) => {
        let neighbors =
          [(x + 1, y), (x, y + 1)]
          ->Belt.Array.map(tuple =>
            switch tuple {
            | (x, y) =>
              try {
                Some(Js.Dict.fromArray([(x->xyToKey(y), xxs[y][x])]))
              } catch {
              | _ => None
              }
            }
          )
          ->Belt.Array.keep(Belt.Option.isSome)
          ->Belt.Array.map(Belt.Option.getExn)
        (x->xyToKey(y), neighbors)
      })
    )
    ->Belt.Array.concatMany
    ->Js.Dict.fromArray
  }

  let getShortestDistanceNodeKey = distances =>
    distances
    ->Js.Dict.keys
    ->Belt.Array.reduce(None, (acc, key) =>
      switch acc {
      | None => Some(key)
      | Some(accKey) =>
        try {
          distances->Js.Dict.unsafeGet(accKey) > distances->Js.Dict.unsafeGet(key)
            ? Some(key)
            : Some(accKey)
        } catch {
        | _ => Js.Exn.raiseError("Could not get node key")
        }
      }
    )
    ->Belt.Option.getExn

  let getNeighbors = (graph, key) =>
    try {
      graph->Js.Dict.get(key)->Belt.Option.getExn
    } catch {
    | _ => Js.Exn.raiseError("Error getting neighbors")
    }

  let updateDistancesAndParents = (distances, parents, neighbors, key) => {
    let keyDistance = try {
      distances->Js.Dict.unsafeGet(key)
    } catch {
    | _ => Js.Exn.raiseError(`Could not get current distance for ${key}`)
    }
    neighbors
    ->Js.Dict.entries
    ->Belt.Array.reduce((distances, parents), (acc, neighbor) =>
      switch neighbor {
      | (neighborKey, neighborDistance) => {
          let (distances, parents) = acc
          let currDistance = try {
            distances->Js.Dict.unsafeGet(neighborKey) +. keyDistance
          } catch {
          | _ => Js.Exn.raiseError("Error getting current distance of neighbor")
          }
          switch neighborDistance +. keyDistance {
          | x if x < currDistance => {
              distances->Js.Dict.set(neighborKey, neighborDistance +. keyDistance)
              parents->Js.Dict.set(neighborKey, key)
              (distances, parents)
            }
          | _ => (distances, parents)
          }
        }
      }
    )
  }

  let rec dijkstra = (graph, start, end) => {
    let parents = Js.Dict.empty()
    let distances =
      graph
      ->Js.Dict.keys
      ->Belt.Array.reduce(Js.Dict.empty(), (acc, key) =>
        switch key {
        | x if x == start => {
            acc->Js.Dict.set(x, 0.)
            acc
          }
        | x => {
            acc->Js.Dict.set(x, Pervasives.infinity)
            acc
          }
        }
      )
    dijkstraInner(graph, end, parents, distances)
  }
  and dijkstraInner = (graph, end, parents, distances) => {
    let shortestDistanceKey = getShortestDistanceNodeKey(distances)
    switch shortestDistanceKey {
    | x if x == end =>
      try {
        distances->Js.Dict.get(x)->Belt.Option.getExn
      } catch {
      | _ => Js.Exn.raiseError("Error getting final distance")
      }
    | x => {
        let neighbors = graph->getNeighbors(shortestDistanceKey)
        let (newDistances, newParents) =
          distances->updateDistancesAndParents(parents, neighbors, shortestDistanceKey)
        graph->dijkstraInner(end, parents, distances)
      }
    }
  }

  try {
    let input = testFile->processInput
    let start = 0->xyToKey(0)
    let end =
      (input->Belt.Array.get(0)->Belt.Option.getExn->Belt.Array.length - 1)
        ->xyToKey(input->Belt.Array.length - 1)
    input->inputToGraph->Js.log
  } catch {
  | Js.Exn.Error(n) => n->Js.log2("error")
  }
}
