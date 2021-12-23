let testFile = Node.Path.resolve("src/day-21", "test-input.txt")
type player = {
  id: string,
  position: int,
  score: int,
}

type playerState = array<player>

type gameState = {
  dieNum: int,
  turn: int,
}

type gameRules = {
  winningScore: int,
  dieSides: int,
  boardSpaces: int,
  rollsPerTurn: int,
}

let processInput = filename => {
  filename
  ->Node.Fs.readFileAsUtf8Sync
  ->Js.String2.split("\n")
  ->Belt.Array.map(str =>
    str
    ->Js.String2.match_(%re("/Player (?<number>\d+) starting position: (?<position>\d+)/"))
    ->Belt.Option.getWithDefault([])
    ->{
      arr =>
        switch arr {
        | [_, player, position] => (player, position->Belt.Int.fromString->Belt.Option.getExn)
        | _ => Js.Exn.raiseError("could not get player / position")
        }
    }
  )
}

let rec sum = ls =>
  switch ls {
  | list{head, ...tail} => head + sum(tail)
  | _ => 0
  }

let rec roll = (~currentNumber=?, ~maxNumber=100, ~numTimes=1, ()) => {
  rollInternal(currentNumber->Belt.Option.getWithDefault(0), maxNumber, numTimes, list{})
}
and rollInternal = (currentNumber, maxNumber, numTimes, acc) => {
  switch numTimes {
  | 0 => (currentNumber, acc)
  | _ => {
      let rolledNumber = switch currentNumber {
      | x if x == maxNumber => 1
      | x => x + 1
      }
      rollInternal(rolledNumber, maxNumber, numTimes - 1, list{rolledNumber, ...acc})
    }
  }
}

let rec playGame = (playerState, gameState, gameRules) => {
  let (newPlayerState, newGameState) = playerState->Belt.Array.reduceWithIndex(([], gameState), (
    (playersArr, gameState),
    player,
    idx,
  ) => {
    let (curr, nums) = roll(~currentNumber=gameState.dieNum, ~numTimes=3, ())
    let sumOfRolls = nums->sum
    let newPosition = mod(player.position + sumOfRolls, gameRules.boardSpaces)->{
      num =>
        switch num {
        | 0 => 10
        | x => x
        }
    }
    let newPlayerRecord = {
      ...player,
      position: newPosition,
      score: player.score + newPosition,
    }
    let newGameState = {
      dieNum: curr,
      turn: idx == 0 ? gameState.turn + 1 : gameState.turn,
    }
    (playersArr->Belt.Array.concat([newPlayerRecord]), newGameState)
  })

  let winners = newPlayerState->Belt.Array.keep(player => player.score >= gameRules.winningScore)

  switch winners->Belt.Array.length {
  | 0 => playGame(newPlayerState, newGameState, gameRules)
  | _ => (newPlayerState, newGameState)
  }
}

let createStartingPlayerState = players =>
  players->Belt.Array.map(player =>
    switch player {
    | (playerId, position) => {id: playerId, position: position, score: 0}
    }
  )

let _ = {
  try {
    let rules: gameRules = {
      boardSpaces: 10,
      dieSides: 100,
      rollsPerTurn: 3,
      winningScore: 1000,
    }
    let startingGameState = {
      dieNum: 0,
      turn: 0,
    }
    testFile
    ->processInput
    ->createStartingPlayerState
    ->playGame(startingGameState, rules)
    ->{
      ((playerState, gameState)) => {
        playerState->Js.log2(`: player state after ${gameState.turn->Belt.Int.toString} turn(s)`)
      }
    }
  } catch {
  | _ => Js.log("something went wrong")
  }
}
