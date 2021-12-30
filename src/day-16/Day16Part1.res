let _ = {
  let testFile = Node.Path.resolve("src/day-16", "test-input.txt")
  let tap = (val, tag) => {
    val->Js.log2(tag)
    val
  }

  let processInput = filename => filename->Node.Fs.readFileAsUtf8Sync

  let hexCharToNumber = char => {
    let possibilities = "0123456789ABCDEF"
    possibilities
    ->Js.String2.split("")
    ->Js.Array2.findIndex(hexChar => char == hexChar)
    ->{
      idx =>
        switch idx {
        | -1 => Js.Exn.raiseError("out of range number")
        | x => x
        }
    }
  }

  let rec padLeft = (str, char, upTo) =>
    switch upTo - str->Js.String2.length {
    | x if x > 0 => (char ++ str)->padLeft(char, upTo)
    | _ => str
    }

  let rec numberToBinaryString: int => string = num => {
    switch num {
    | 0 => ""
    | num => {
        let quotient = num / 2
        let remainder = Pervasives.mod(num, 2)
        quotient->numberToBinaryString ++ remainder->Belt.Int.toString
      }
    }
  }

  let rec binaryStringToDecimal: string => int = binary =>
    switch binary->Js.String2.charAt(0) {
    | "" => 0
    | x if x == "0" || x == "1" =>
      x
      ->tap("x")
      ->Belt.Float.fromString
      ->Belt.Option.getExn
      ->(num =>
        num *. Js.Math.pow_float(~base=2., ~exp=binary->Js.String.length->Belt.Float.fromInt -. 1.))
      ->tap("after float")
      ->Belt.Int.fromFloat + binary->Js.String2.sliceToEnd(~from=1)->binaryStringToDecimal

    | _ => Js.Exn.raiseError("characters must be 1 or 0")
    }

  //   let trimLeft = (string, char) => switch string->Js.String2.charAt(string->Js.String2.length - 1) {
  //       | x if x == char => string->Js.String2.slice(0, string->Js.String2.length - 2)->trimLeft(char)
  //       | _ =>
  //   }

  let rec parsePacket = packet => {
    let version = packet->Js.String2.slice(~from=0, ~to_=3)->binaryStringToDecimal
    let typeId = packet->Js.String2.slice(~from=3, ~to_=6)->binaryStringToDecimal
    let packetDecimal = packet->Js.String2.sliceToEnd(~from=6)->parseLiteral("")
    (version, typeId, packetDecimal)
  }
  and parseLiteral = (str, acc) =>
    switch str->Js.String2.charAt(0) {
    | "1" => {
        let newAcc = acc ++ str->Js.String2.slice(~from=1, ~to_=5)
        str->Js.String2.sliceToEnd(~from=5)->parseLiteral(newAcc)
      }
    | "0" => (acc ++ str->Js.String2.slice(~from=1, ~to_=5))->binaryStringToDecimal
    | x => Js.Exn.raiseError(`Literal parse failed with: ${x}`)
    }

  let hexToBinary = hex =>
    hex
    ->Js.String2.split("")
    ->Belt.Array.map(char =>
      char
      //   ->tap("after char")
      ->hexCharToNumber
      ->numberToBinaryString
      //   ->tap(": binary")
      ->padLeft("0", 4)
    )
    //   ->tap(": pad")
    ->Js.Array2.joinWith("")

  testFile->processInput->hexToBinary->parsePacket->Js.log
}
