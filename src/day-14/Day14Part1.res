let testFile = Node.Path.resolve("src/day-14", "test-input.txt")
let realFile = Node.Path.resolve("src/day-14", "input.txt")
let file = realFile

let getInput = filename => filename->Node.Fs.readFileAsUtf8Sync

let getInitialTemplate = input => input->Js.String2.split("\n\n")

let initialTemplate =
  file
  ->getInput
  ->getInitialTemplate
  ->Belt.Array.get(0)
  ->Belt.Option.getWithDefault("")
  ->Js.String2.split("")
  ->Belt.List.fromArray

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

let rec processTemplateWithRules = (ls, rules, times) => {
  switch times {
  | 0 => ls
  | _ => {
      let result = iterateWithRules(ls, rules)
      processTemplateWithRules(result, rules, times - 1)
    }
  }
}
and iterateWithRules = (ls, rules) => {
  switch ls {
  | list{first, second, ...rest} => {
      let tuple = rules->Belt.Array.getBy(x =>
        switch x {
        | (before, after, _) if before == first && after == second => true
        | _ => false
        }
      )
      switch tuple {
      | Some(_, _, insert) => list{first, insert, ...iterateWithRules(list{second, ...rest}, rules)}
      | _ => list{first, ...iterateWithRules(list{second, ...rest}, rules)}
      }
    }
  | _ => ls
  }
}

let eq = (a, b) => a == b

let countCharactersInList = ls => {
  ls->Belt.List.reduce(list{}, (acc, letter) => {
    let assoc = acc->Belt.List.getAssoc(letter, eq)
    switch assoc {
    | Some(num) => acc->Belt.List.setAssoc(letter, num + 1, eq)
    | None => acc->Belt.List.setAssoc(letter, 1, eq)
    }
  })
}

let getAnswer = ls => {
  let max =
    ls
    ->Belt.List.map(item => {
      let (_, num) = item
      num
    })
    ->Belt.List.toArray
    ->Js.Math.maxMany_int
  let min =
    ls
    ->Belt.List.map(item => {
      let (_, num) = item
      num
    })
    ->Belt.List.toArray
    ->Js.Math.minMany_int
  max - min
}

let list = initialTemplate->processTemplateWithRules(insertionRules, 10)

let characterCount = list->countCharactersInList

characterCount->getAnswer->Js.log2("is the answer")
