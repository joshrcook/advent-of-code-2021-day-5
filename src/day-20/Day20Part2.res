let testFile = Node.Path.resolve("src/day-20", "test-input.txt")
let realFile = Node.Path.resolve("src/day-20", "input.txt")
type math
@val external math: math = "Math"
@send external pow: (math, int, int) => int = "pow"
exception BadArgument(string)

// for conversions
let oneChar = "#"
let zeroChar = "."

let _ = {
  let powInt = (base: int, power: int): int => {
    switch base {
    | x if x >= 0 =>
      switch power {
      | x if x >= 0 => math->pow(base, power)
      | _ => raise(BadArgument("power must be a postive number"))
      }
    | _ => raise(BadArgument("base must be a positive number"))
    }
  }

  let convertBinaryString: (string, string, string) => string = (str, zeroChar, oneChar) => {
    str
    ->Js.String2.split("")
    ->Belt.Array.map(val =>
      switch val {
      | x if x == oneChar => "1"
      | x if x == zeroChar => "0"
      | _ => "0"
      }
    )
    ->Js.Array2.joinWith("")
  }

  let rec binaryStringToBinary: string => int = str => {
    let len = str->Js.String2.length
    switch len {
    | 0 => 0
    | _ =>
      switch str->Js.String2.charAt(0) {
      | x if x == "0" => 0 + binaryStringToBinary(str->Js.String2.sliceToEnd(~from=1))
      | x if x == "1" =>
        1 * powInt(10, len - 1) + binaryStringToBinary(str->Js.String2.sliceToEnd(~from=1))
      | x => raise(BadArgument(`binaryCharsToBinary requires only zero and one chars, found: ${x}`))
      }
    }
  }

  let binaryToDecimal: int => int = binary =>
    binary
    ->Belt.Int.toString
    ->Js.String2.split("")
    ->Belt.Array.reverse
    ->Belt.Array.map(item =>
      item->Belt.Int.fromString->Belt.Option.getWithDefault(0)->Belt.Int.toFloat
    )
    ->Belt.Array.reduceWithIndex(0., (acc, item, idx) => {
      let pow = Js.Math.pow_float(~base=2., ~exp=idx->Belt.Int.toFloat)
      acc +. item *. pow
    })
    ->Belt.Int.fromFloat

  let processInput = (filename, zeroChar, oneChar) =>
    filename
    ->Node.Fs.readFileAsUtf8Sync
    ->Js.String2.split("\n\n")
    ->{
      arr => {
        try {
          let algo =
            arr[0]
            ->Js.String2.split("\n")
            ->Js.Array2.joinWith("")
            ->convertBinaryString(zeroChar, oneChar)
          let starter =
            arr[1]
            ->Js.String2.split("\n")
            ->Belt.Array.map(str =>
              str
              ->Js.String2.split("")
              ->Belt.Array.map(str => str->convertBinaryString(zeroChar, oneChar))
            )
          (algo, starter)
        } catch {
        | _ => Js.Exn.raiseError("input not correctly processed")
        }
      }
    }

  let surround: (array<array<'a>>, 'a) => array<array<'a>> = (matrix, infinityChar) => {
    try {
      let newMatrix = matrix->Belt.Array.map(arr => {
        let _ = arr->Js.Array2.push(infinityChar)
        let _ = arr->Js.Array2.unshift(infinityChar)
        arr
      })
      let matrixLen = newMatrix[0]->Belt.Array.length
      let rowOfDots = Belt.Array.make(matrixLen, infinityChar)
      let _ = newMatrix->Js.Array2.unshift(rowOfDots->Belt.Array.copy)
      let _ = newMatrix->Js.Array2.push(rowOfDots->Belt.Array.copy)
      newMatrix
    } catch {
    | _ => Js.Exn.raiseError("Image prep failed")
    }
  }

  let enhance = (image, algo, infinityChar) => {
    image
    ->surround(infinityChar)
    ->{
      preppedImage => {
        preppedImage->Belt.Array.mapWithIndex((y, arr) =>
          arr
          ->Belt.Array.mapWithIndex((x, item) => {
            try {
              let characterLocations = list{
                (x - 1, y - 1),
                (x, y - 1),
                (x + 1, y - 1),
                (x - 1, y),
                (x, y),
                (x + 1, y),
                (x - 1, y + 1),
                (x, y + 1),
                (x + 1, y + 1),
              }
              characterLocations
              ->Belt.List.map(tuple =>
                switch tuple {
                | (x, y) =>
                  try {
                    preppedImage[y][x]
                  } catch {
                  | _ => infinityChar
                  }
                }
              )
              ->Belt.List.toArray
              ->Js.Array2.joinWith("")
              ->binaryStringToBinary
              ->binaryToDecimal
              ->{
                decimal => {
                  (item, algo->Js.String2.charAt(decimal))
                }
              }
            } catch {
            | _ => (item, item)
            }
          })
          ->Belt.Array.map(val =>
            switch val {
            | (_, newVal) => newVal
            }
          )
        )
      }
    }
  }

  let matrixCount = (matrix, character) =>
    matrix
    ->Belt.Array.concatMany
    ->Belt.Array.reduce(0, (acc, item) =>
      switch item {
      | x if x == character => acc + 1
      | _ => acc
      }
    )

  let rec processSteps = (image, algo, infinityCharacter, steps) => {
    switch steps {
    | 0 => image
    | _ => {
        let newImage = image->enhance(algo, infinityCharacter)
        let nextStep = steps - 1
        let nextInfinityCharPosition =
          9
          ->Belt.Array.make(infinityCharacter)
          ->Js.Array2.joinWith("")
          ->binaryStringToBinary
          ->binaryToDecimal
        let nextInfinityChar = algo->Js.String2.charAt(nextInfinityCharPosition)
        newImage->processSteps(algo, nextInfinityChar, nextStep)
      }
    }
  }

  try {
    realFile
    ->processInput(zeroChar, oneChar)
    ->{
      ((algo, starter)) => {
        algo->Js.log
        starter->processSteps(algo, "0", 50)->matrixCount("1")->Js.log2("is the answer")
      }
    }
  } catch {
  | Js.Exn.Error(err) => err->Js.Exn.message->Js.log2("error")
  }
}
