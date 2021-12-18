type probe = {"position": (int, int), "velocity": (int, int)}
type range = {"x": (int, int), "y": (int, int)}
let testFile = Node.Path.resolve("src/day-17", "test-input.txt")

let getInput: string => range = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.replace("target area: x=", "")
  ->Js.String2.replace(" y=", "")
  ->Js.String2.split(",")
  ->Belt.Array.map(str =>
    str
    ->Js.String2.split("..")
    ->Belt.Array.map(Belt.Int.fromString)
    ->{
      arr =>
        switch arr {
        | [Some(min), Some(max)] => (min, max)
        | _ => Js.Exn.raiseError("nope")
        }
    }
  )
  ->{
    arr =>
      switch arr {
      | [(xmin, xmax), (ymin, ymax)] => {
          "x": (xmin, xmax),
          "y": (ymin, ymax),
        }
      | _ => Js.Exn.raiseError("nada")
      }
  }

let step: probe => probe = probe => {
  let (xpos, ypos) = probe["position"]
  let (xvel, yvel) = probe["velocity"]
  let grav = yvel - 1
  let newProbe = {
    "position": (xpos + xvel, ypos + yvel),
    "velocity": switch xvel {
    | x if x > 0 => (xvel - 1, grav)
    | x if x < 0 => (xvel + 1, grav)
    | _ => (xvel, grav)
    },
  }
  newProbe
}

let rec test: (range, probe) => bool = (range, probe) => {
  probe
  ->step
  ->{
    probe => {
      let (xmin, xmax) = range["x"]
      let (ymin, ymax) = range["y"]
      switch probe["position"] {
      | (xpos, ypos) if xpos >= xmin && xpos <= xmax && ypos >= ymin && ypos <= ymax => true
      | (xpos, ypos) if xpos > xmax || ypos < ymin => false
      | _ => test(range, probe)
      }
    }
  }
}

let makeProbe = (initialXVelocity, initialYVelocity): probe => {
  {
    "position": (0, 0),
    "velocity": (initialXVelocity, initialYVelocity),
  }
}

let rec testX: (range, probe) => bool = (range, probe) => {
  probe
  ->step
  ->{
    probe => {
      let (xmin, xmax) = range["x"]
      let (xvelocity, _) = probe["velocity"]
      switch probe["position"] {
      | (xpos, _) if xpos >= xmin && xpos <= xmax => true
      | (xpos, _) if xpos > xmax => false
      | (xpos, _) if xpos < xmin && xvelocity != 0 => testX(range, probe)
      | _ => false
      }
    }
  }
}

let rec findPotentialXVelocities: range => array<int> = range => {
  let (_, xmax) = range["x"]
  range->findXInner(xmax, [])
}
and findXInner = (range, testNum, values) => {
  switch testNum {
  | 0 => values->Belt.Array.reverse
  | _ => {
      let testProbe = makeProbe(testNum, 0)
      let testResult = range->testX(testProbe)
      switch testResult {
      | true => findXInner(range, testNum - 1, values->Belt.Array.concat([testNum]))
      | false => findXInner(range, testNum - 1, values)
      }
    }
  }
}

let _ = {
  try {
    let range = testFile->getInput
    range->findPotentialXVelocities->Js.log
  } catch {
  | _ => Js.log("something went wrong")
  }
}
