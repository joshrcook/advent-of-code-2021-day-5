let testFile = Node.Path.resolve("src/day-14", "test-input.txt")
let realFile = Node.Path.resolve("src/day-14", "input.txt")
let file = realFile

let fileD = file->Node.Fs.openSync

let getInput = filename => filename->Node.Fs.readFileAsUtf8Sync

let getInitialTemplate = input => input->Js.String2.split("\n\n")

let initialTemplate =
  file->getInput->getInitialTemplate->Belt.Array.get(0)->Belt.Option.getWithDefault("")

let insertionRules =
  file
  ->getInput
  ->getInitialTemplate
  ->Belt.Array.get(1)
  ->Belt.Option.getWithDefault("")
  ->Js.String2.split("\n")
  ->Belt.Array.map(str => {
    let split = str->Js.String2.split(" -> ")
    let keys = split->Belt.Array.get(0)->Belt.Option.map(x => x->Js.String2.split(""))
    let value = split->Belt.Array.get(1)
    switch keys {
    | Some([before, after]) =>
      switch value {
      | Some(x) => Some(before, after, x)
      | _ => None
      }
    | _ => None
    }
  })
  ->Belt.Array.keep(Belt.Option.isSome)
  ->Belt.Array.map(Belt.Option.getExn)

let rec processTemplateWithRules = (str, rules, times) => {
  switch times {
  | 0 => str
  | _ => {
      let result = processInternal(str, rules, "")
      processTemplateWithRules(result, rules, times - 1)
    }
  }
}
and processInternal = (str, rules, acc) => {
  let first = str->Js.String2.charAt(0)
  let second = str->Js.String2.charAt(1)
  switch second {
  | "" => acc->Js.String2.concat(first)
  | _ => {
      let tuple = rules->Js.Array2.find(x =>
        switch x {
        | (before, after, _) if before == first && after == second => true
        | _ => false
        }
      )
      let next = str->Js.String2.sliceToEnd(~from=1)
      switch tuple {
      | Some(_, _, x) =>
        processInternal(next, rules, acc->Js.String2.concat(first)->Js.String2.concat(x))
      | _ => processInternal(next, rules, acc->Js.String2.concat(first))
      }
    }
  }
}

let eq = (a, b) => a == b

let rec countCharactersInString = str => {
  countCharactersInternal(str, Js.Dict.empty())
}
and countCharactersInternal = (str, dict) => {
  switch str {
  | "" => dict
  | _ => {
      let character = str->Js.String2.charAt(0)
      let assoc = dict->Js.Dict.get(character)
      switch assoc {
      | Some(num) => dict->Js.Dict.set(character, num + 1)
      | None => dict->Js.Dict.set(character, 1)
      }
      countCharactersInternal(str->Js.String2.sliceToEnd(~from=1), dict)
    }
  }
}

let getAnswer = dict => {
  let max = dict->Js.Dict.values->Js.Math.maxMany_int
  let min = dict->Js.Dict.values->Js.Math.minMany_int
  max - min
}

// initialTemplate->Js.log
// insertionRules->Js.log

let str = initialTemplate->processTemplateWithRules(insertionRules, 10)

str->Js.log

// let characterCount = str->countCharactersInString

// characterCount->Js.log

// characterCount->getAnswer->Js.log2("is the answer")
