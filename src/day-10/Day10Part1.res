let testFile = Node.Path.resolve("src/day-10", "test-input.txt")
let realFile = Node.Path.resolve("src/day-10", "input.txt")

let processInput = filename =>
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.splitByRe(%re("/\\r?\\n/"))
  ->Js.Array2.filter(Belt.Option.isSome)
  ->Js.Array2.map(Belt.Option.getExn)
  ->Js.Array2.map(str => str->Js.String2.split("")->Belt.List.fromArray)

let assoc = list{("(", ")"), ("[", "]"), ("{", "}"), ("<", ">")}

let rec hasInvalidCharacters = ls => {
  validateStringInner(ls, list{})
}
and validateStringInner = (ls, acc) => {
  switch ls {
  | list{first, ...rest} =>
    switch first {
    | "("
    | "["
    | "{"
    | "<" =>
      validateStringInner(rest, list{first, ...acc})
    | ")"
    | "]"
    | "}"
    | ">" => {
        let head = acc->Belt.List.head->Belt.Option.getWithDefault("")
        let tail = acc->Belt.List.tail->Belt.Option.getWithDefault(list{})
        let matches =
          assoc->Belt.List.getAssoc(head, (a, b) => a == b)->Belt.Option.getWithDefault("") == first
        switch matches {
        | true => validateStringInner(rest, tail)
        | false => Some(first)
        }
      }
    | _ => validateStringInner(rest, acc)
    }
  | _ => None
  }
}

realFile
->processInput
// determine errant characters
->Js.Array2.map(hasInvalidCharacters)
->Js.Array2.filter(Belt.Option.isSome)
->Js.Array2.map(Belt.Option.getExn)
// map characters to scores
->Js.Array2.map(char =>
  switch char {
  | ")" => 3
  | "]" => 57
  | "}" => 1197
  | ">" => 25137
  | _ => 0
  }
)
// sum all scores
->Js.Array2.reduce((acc, num) => acc + num, 0)
->Js.log
