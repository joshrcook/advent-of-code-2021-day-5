let _ = {
  let testFile = Node.Path.resolve("src/day-15", "test-input.txt")
  // let realFile = Node.Path.resolve("src/day-15", "input.txt")

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

  let inputToGraph: array<array<float>> => Js.Dict.t<Js.Dict.t<float>> = xxs => {
    xxs
    ->Belt.Array.mapWithIndex((y, row) =>
      row->Belt.Array.mapWithIndex((x, _) => {
        let neighbors =
          [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
          ->Belt.Array.map(tuple =>
            switch tuple {
            | (x, y) =>
              try {
                Some(x->xyToKey(y), xxs[y][x])
              } catch {
              | _ => None
              }
            }
          )
          ->Belt.Array.keep(Belt.Option.isSome)
          ->Belt.Array.map(Belt.Option.getExn)
          ->Js.Dict.fromArray
        (x->xyToKey(y), neighbors)
      })
    )
    ->Belt.Array.concatMany
    ->Js.Dict.fromArray
  }

  let getShortestDistanceNodeKey = (distances, processed) =>
    distances
    ->Js.Dict.entries
    ->Belt.Array.keep(entry => {
      let (key, _) = entry
      processed->Js.Array2.includes(key) == false
    })
    ->Belt.Array.reduce(None, (acc, entry) =>
      switch acc {
      | None => Some(entry)
      | Some(acc) =>
        let (_, accValue) = acc
        let (_, entryValue) = entry
        accValue < entryValue ? Some(acc) : Some(entry)
      }
    )
    ->{
      entry =>
        switch entry {
        | Some(entry) => {
            let (key, _) = entry
            key
          }
        | None => Js.Exn.raiseError("No key found")
        }
    }

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
          let currDistance = distances->Js.Dict.get(neighborKey)->Belt.Option.getWithDefault(0.)
          let newDistance = neighborDistance +. keyDistance
          switch newDistance {
          | x if x < currDistance => {
              distances->Js.Dict.set(neighborKey, newDistance)
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
    let processed = []
    dijkstraInner(graph, end, parents, distances, processed, 1000)
  }
  and dijkstraInner = (graph, end, parents, distances, processed, max) => {
    let shortestDistanceKey = distances->getShortestDistanceNodeKey(processed)
    // shortestDistanceKey->Js.log2("key")
    switch shortestDistanceKey {
    | key if key == end => (distances, parents)
    | key => {
        // key->Js.log2("key")
        let neighbors = graph->getNeighbors(key)
        // neighbors->Js.log2("neighbors")
        let (newDistances, newParents) =
          distances->updateDistancesAndParents(parents, neighbors, key)

        let _ = processed->Js.Array2.push(key)
        // if max == 0 {
        //   distances
        // } else {
        graph->dijkstraInner(end, newParents, newDistances, processed, max - 1)
      }
    }
  }

  try {
    let startTime = Js.Date.now()
    let input = testFile->processInput
    let start = 0->xyToKey(0)
    let end =
      (input->Belt.Array.get(0)->Belt.Option.getExn->Belt.Array.length - 1)
        ->xyToKey(input->Belt.Array.length - 1)
    input
    ->inputToGraph
    ->dijkstra(start, end)
    ->(tuple =>
      switch tuple {
      | (distances, _) => distances->Js.Dict.get(end)
      })
    ->Js.log
    let endTime = Js.Date.now()
    (endTime -. startTime)->Js.log2("time")
  } catch {
  | Js.Exn.Error(n) => n->Js.log2("error")
  }
}
