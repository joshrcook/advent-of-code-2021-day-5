let _ = {
  let testFile = Node.Path.resolve("src/day-15", "test-input.txt")
  let realFile = Node.Path.resolve("src/day-15", "input.txt")

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

  let getMinNonProcessedIdx = (matrix, processed) => {
    matrix
    ->Belt.Array.length
    ->{
      len => {
        matrix
        ->Belt.Array.reduceWithIndex(None, (accy, row, y) => {
          let accx = row->Belt.Array.reduceWithIndex(None, (accx, num, x) => {
            let isProcessed = try {
              processed[y][x]
            } catch {
            | _ => Js.Exn.raiseError("Could not find index in processed")
            }
            switch isProcessed {
            | true => accx
            | false =>
              switch accx {
              | None => Some(x, y, num)
              | Some(_, _, accnum) => num < accnum ? Some(x, y, num) : accx
              }
            }
          })
          switch accy {
          | None => accx
          | Some(_, _, ynum) =>
            switch accx {
            | None => accy
            | Some(_, _, xnum) => xnum < ynum ? accx : accy
            }
          }
        })
        ->Belt.Option.getExn
        ->{
          tuple =>
            switch tuple {
            | (x, y, _) => (x, y)
            }
        }
      }
    }
  }

  let getNewDistancesAndParents = (graph, distances, parents, key) => {
    switch key {
    | (x, y) => {
        let currentDistance = try {
          distances[y][x]
        } catch {
        | _ => Js.Exn.raiseError("Could not get current distance")
        }
        let neighbors = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]->Belt.Array.reduce([], (
          acc,
          tuple,
        ) =>
          switch tuple {
          | (neighborx, neighbory) =>
            try {
              let _ = acc->Js.Array2.push((neighborx, neighbory, graph[neighbory][neighborx]))
              acc
            } catch {
            | _ => acc
            }
          }
        )
        neighbors->Belt.Array.forEach(neighbor =>
          switch neighbor {
          | (neighborx, neighbory, value) => {
              let neighborCurrentDistance = try {
                distances[neighbory][neighborx]
              } catch {
              | _ => Js.Exn.raiseError("Could not get current distance for neighbor")
              }
              let newDistance = value +. currentDistance
              if newDistance < neighborCurrentDistance {
                try {
                  distances[neighbory][neighborx] = newDistance
                  parents[neighbory][neighborx] = key
                } catch {
                | _ => Js.Exn.raiseError("Error updating distances / parents")
                }
              }
            }
          }
        )
        (distances, parents)
      }
    }
  }

  // graph is assuming matrix...
  let rec dijkstra = graph => {
    let parents = graph->Belt.Array.map(row => row->Belt.Array.map(_ => (0, 0)))
    let distances = graph->Belt.Array.mapWithIndex((y, row) =>
      row->Belt.Array.mapWithIndex((x, _) =>
        switch (x, y) {
        | (0, 0) => 0.
        | _ => Pervasives.infinity
        }
      )
    )
    let processed = graph->Belt.Array.map(row => row->Belt.Array.map(_ => false))
    let len = graph->Belt.Array.length
    dijkstraInner(graph, len, distances, parents, processed)
  }
  and dijkstraInner = (graph, len, distances, parents, processed) => {
    let key = distances->getMinNonProcessedIdx(processed)
    switch key {
    | (x, y) if x == len - 1 && y == len - 1 =>
      try {
        (graph, distances, parents)
      } catch {
      | _ => Js.Exn.raiseError("Could not get final distance")
      }
    | (x, y) => {
        let (distances, parents) = graph->getNewDistancesAndParents(distances, parents, key)
        processed[y][x] = true
        graph->dijkstraInner(len, distances, parents, processed)
      }
    }
  }

  let matrixToString = matrix =>
    matrix->Belt.Array.map(row => row->Js.Array2.joinWith(""))->Js.Array2.joinWith("\n")

  // let rec answerToString = (graph, parents) => {
  //   let stringAcc = graph->Belt.Array.map(row => row->Belt.Array.map(_ => "-"))
  //   let parentsLen = parents->Belt.Array.length
  //   let startingKey = (parentsLen - 1, parentsLen - 1)
  //   graph->answerToStringInner(parents, stringAcc, startingKey)
  // }
  // and answerToStringInner = (graph, parents, acc, key) => {
  //   switch key {
  //   | (0, 0) => acc->matrixToString
  //   | (x, y) => {
  //       try {
  //         acc[y][x] = graph[y][x]->Belt.Float.toString
  //       } catch {
  //       | _ => Js.Exn.raiseError("Could not update string matrix")
  //       }
  //       let parent = try {
  //         parents[y][x]
  //       } catch {
  //       | _ => Js.Exn.raiseError("Could not get parent")
  //       }
  //       graph->answerToStringInner(parents, acc, parent)
  //     }
  //   }
  // }

  let rec expandMatrix = (initialMatrix, times) => {
    initialMatrix->expandMatrixInternal(times - 1, times - 1, initialMatrix->Belt.Array.length)
  }
  and mapper = num =>
    switch num +. 1. {
    | 10. => 1.
    | x => x
    }
  and expandMatrixInternal = (acc, timesy, timesx, initialLen) => {
    switch timesy {
    | 0 =>
      switch timesx {
      | 0 => acc
      | x if x > 0 => {
          let newAcc = acc->Belt.Array.map(row => {
            let sliceToAdd = row->Belt.Array.sliceToEnd(initialLen * -1)->Belt.Array.map(mapper)
            row->Belt.Array.concat(sliceToAdd)
          })
          newAcc->expandMatrixInternal(timesy, timesx - 1, initialLen)
        }
      | _ => Js.Exn.raiseError("timesx must be a postitive number")
      }
    | y if y > 0 => {
        let sliceToAdd =
          acc
          ->Belt.Array.sliceToEnd(initialLen * -1)
          ->Belt.Array.map(row => row->Belt.Array.map(mapper))
        let newAcc = acc->Belt.Array.concat(sliceToAdd)
        newAcc->expandMatrixInternal(timesy - 1, timesx, initialLen)
      }
    | _ => Js.Exn.raiseError("Times y should be greater than 0")
    }
  }

  try {
    let startTime = Js.Date.now()
    realFile
    ->processInput
    ->expandMatrix(5)
    ->dijkstra
    ->{
      tuple =>
        switch tuple {
        | (_, distances, _) =>
          // graph->answerToString(parents)->Js.log
          // distances->Js.log2("distances")
          // parents->Js.log2("parents")
          distances
          ->Belt.Array.length
          ->{
            len =>
              try {
                distances[len - 1][len - 1]->Js.log2("dijkstra")
              } catch {
              | _ => Js.Exn.raiseError("nope")
              }
          }
        }
    }

    let endTime = Js.Date.now()
    (endTime -. startTime)->Js.log2("time")
  } catch {
  | Js.Exn.Error(n) => n->Js.log2("error")
  }
}
