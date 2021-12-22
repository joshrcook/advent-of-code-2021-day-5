let testFile = Node.Path.resolve("src/day-20", "test-input.txt")
let realFile = Node.Path.resolve("src/day-20", "input.txt")

let processInput = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n\n")
  ->{
    arr => {
      try {
        let algo = arr[0]->Js.String2.split("\n")->Js.Array2.joinWith("")
        let starter =
          arr[1]->Js.String2.split("\n")->Belt.Array.map(str => str->Js.String2.split(""))
        (algo, starter)
      } catch {
      | _ => Js.Exn.raiseError("input not correctly processed")
      }
    }
  }

let prep = image => {
  try {
    let newImage = image->Belt.Array.map(arr => {
      let _ = arr->Js.Array2.push(".")
      let _ = arr->Js.Array2.unshift(".")
      arr
    })
    let imageLen = newImage[0]->Belt.Array.length
    let rowOfDots = Belt.Array.make(imageLen, ".")
    let _ = newImage->Js.Array2.unshift(rowOfDots->Belt.Array.copy)
    let _ = newImage->Js.Array2.push(rowOfDots->Belt.Array.copy)
    newImage
  } catch {
  | _ => Js.Exn.raiseError("Image prep failed")
  }
}

let binaryToDecimal = binary =>
  binary
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

let enhance = (image, algo) => {
  image->{
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
                | _ => "."
                }
              }
            )
            ->Belt.List.map(char =>
              switch char {
              | "#" => "1"
              | _ => "0"
              }
            )
            ->Belt.List.toArray
            ->Js.Array2.joinWith("")
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

let matrixToString = matrix =>
  matrix->Belt.Array.map(arr => arr->Js.Array2.joinWith(""))->Js.Array2.joinWith("\n")

let matrixCount = (matrix, character) =>
  matrix
  ->Belt.Array.concatMany
  ->Belt.Array.reduce(0, (acc, item) =>
    switch item {
    | x if x == character => acc + 1
    | _ => acc
    }
  )

// I have a problem because the external stuff is blinking
// I think I can solve it by using the algorithm to find what the all 0's value is going to be as well as the all 1's value
// Basically, I need to read the algorithm to understand what those values should be
// If the algorithm has a . at the 0 position - easy - all dots infinitely
// If the algorithm has a # at the 0 position, I need to tell what the 111111111 (512 in decimal) value is.  If that's a ., it will flash.
// If that's a # though, it will just stay on, which wouldn't make much sense when counting #'s...

let _ = {
  try {
    realFile
    ->processInput
    ->{
      ((algo, starter)) => {
        algo->Js.String2.length->Js.log
        starter->matrixToString->Js.log
        starter->enhance(algo)->enhance(algo)->matrixCount("#")->Js.log2("is the answer")
      }
    }
  } catch {
  | Js.Exn.Error(err) => err->Js.Exn.message->Js.log2("error")
  }
}
