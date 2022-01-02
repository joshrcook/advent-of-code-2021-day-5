type onOff = Off | On
type rule = {
  onOff: onOff,
  xStart: int,
  xEnd: int,
  yStart: int,
  yEnd: int,
  zStart: int,
  zEnd: int,
}

let _ = {
  let testFile = Node.Path.resolve("src/day-22", "test-input.txt")
  let testFile2 = Node.Path.resolve("src/day-22", "test-input2.txt")

  let createCube = (length, initialValue) =>
    Belt.Array.make(length, Belt.Array.make(length, initialValue))

  let re = %re("/(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/")

  let processInput = filename =>
    filename
    ->Node.Fs.readFileAsUtf8Sync
    ->Js.String2.split("\n")
    ->Belt.Array.map(rule => {
      rule->Js.log2("rule")
      re
      ->Js.Re.exec_(rule)
      ->{
        option =>
          switch option {
          | None => Js.Exn.raiseError("Rules could not be parsed")
          | Some(res) =>
            switch res
            ->Js.Re.captures
            ->Belt.Array.map(capture => capture->Js.Nullable.toOption->Belt.Option.getExn) {
            | [_, onOff, xStart, xEnd, yStart, yEnd, zStart, zEnd] =>
              try {
                {
                  onOff: switch onOff {
                  | "on" => On
                  | "off" => Off
                  | _ => Js.Exn.raiseError("Can only be on or off")
                  },
                  xStart: xStart->Belt.Int.fromString->Belt.Option.getExn,
                  xEnd: xEnd->Belt.Int.fromString->Belt.Option.getExn,
                  yStart: yStart->Belt.Int.fromString->Belt.Option.getExn,
                  yEnd: yEnd->Belt.Int.fromString->Belt.Option.getExn,
                  zStart: zStart->Belt.Int.fromString->Belt.Option.getExn,
                  zEnd: zEnd->Belt.Int.fromString->Belt.Option.getExn,
                }
              } catch {
              | _ => Js.Exn.raiseError("Error parsing result to result record")
              }
            | _ => Js.Exn.raiseError("captures not correctly formatted")
            }
          }
      }
    })

  let modulateRule = (rule, by) => {
    ...rule,
    xStart: rule.xStart + by,
    xEnd: rule.xEnd + by,
    yStart: rule.yStart + by,
    yEnd: rule.yEnd + by,
    zStart: rule.zStart + by,
    zEnd: rule.zEnd + by,
  }

  let isValidRule = (rule, min, max) => {
    let {xStart, xEnd, yStart, yEnd, zStart, zEnd} = rule
    switch xEnd < min || xStart > max || yStart > max || yEnd < min || zStart > max || zEnd < min {
    | true => false
    | false => true
    }
  }

  let sumArray = xs => xs->Belt.Array.reduce(0, (acc, x) => acc + x)

  let sum3d = cube =>
    cube->Belt.Array.map(x =>
      x->Belt.Array.map(y =>
        y->Belt.Array.map(z =>
          switch z {
          | On => 1
          | Off => 0
          }
        )
      )
    )

  let processRules = (rules, cube) => {
    let modulate = 50
    let min = 0
    let max = 100
    rules
    ->Belt.Array.keep(rule => rule->isValidRule(-50, 50))
    ->Belt.Array.map(rule => rule->modulateRule(50))
    ->Belt.Array.forEach(rule => {
      let {xStart, xEnd, yStart, yEnd, zStart, zEnd, onOff} = rule
      let xMin = Js.Math.max_int(xStart, min)
      let xMax = Js.Math.min_int(xEnd, max)
      let yMin = Js.Math.max_int(yStart, min)
      let yMax = Js.Math.min_int(yEnd, max)
      let zMin = Js.Math.max_int(zStart, min)
      let zMax = Js.Math.min_int(zEnd, max)
    })
  }

  try {
    let cube = createCube(101, Off)
    testFile->processInput->processRules(cube)->Js.log
  } catch {
  | Js.Exn.Error(x) =>
    x->Js.Exn.message->Belt.Option.getWithDefault("Default error message")->Js.log2("error")
  }
}
