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

let rec getRemainingCharacters = ls => {
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
        | false => None
        }
      }
    | _ => validateStringInner(rest, acc)
    }
  | _ => Some(acc)
  }
}

realFile
->processInput
// get remaining characters
->Js.Array2.map(getRemainingCharacters)
->Js.Array2.filter(Belt.Option.isSome)
->Js.Array2.map(Belt.Option.getExn)
// associate characters to their keys
->Js.Array2.map(ls =>
  ls
  ->Belt.List.map(char => assoc->Belt.List.getAssoc(char, (a, b) => a == b))
  ->Belt.List.keep(Belt.Option.isSome)
  ->Belt.List.map(Belt.Option.getExn)
)
// reduce the lists to their appropriate score
->Js.Array2.map(ls =>
  ls->Belt.List.reduce(0., (acc, item) =>
    switch item {
    | ")" => acc *. 5. +. 1.
    | "]" => acc *. 5. +. 2.
    | "}" => acc *. 5. +. 3.
    | ">" => acc *. 5. +. 4.
    | _ => acc
    }
  )
)
// sort the list
->Js.Array2.sortInPlaceWith((a, b) =>
  switch a -. b {
  | x if x < 0. => -1
  | x if x > 0. => 1
  | 0.
  | _ => 0
  }
)
// get the middle value
->(arr => arr->Belt.Array.get(arr->Belt.Array.length / 2)->Belt.Option.getExn)
->Js.log2("is the number you need")
