let testFile = Node.Path.resolve("src/day-21", "test-input.txt")
let realFile = Node.Path.resolve("src/day-21", "input.txt")

type player = {
  id: string,
  position: int,
  score: int,
}

type playerState = array<player>

type gameState = {
  dieRolls: int,
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

let rec roll = (timesRolled, ~maxNumber=100, ~numTimes=1, ()) => {
  rollInternal(timesRolled, maxNumber, numTimes, list{})
}
and rollInternal = (timesRolled, maxNumber, numTimes, acc) => {
  switch numTimes {
  | 0 => (timesRolled, acc)
  | _ => {
      let rolledNumber = mod(timesRolled, maxNumber) + 1
      rollInternal(timesRolled + 1, maxNumber, numTimes - 1, list{rolledNumber, ...acc})
    }
  }
}

let rec playGame = (playerState, gameState, gameRules) => {
  let (newPlayerState, newGameState) = playerState->Belt.Array.reduceWithIndex(([], gameState), (
    (playersArr, gameState),
    player,
    idx,
  ) => {
    let hasWinner =
      playersArr->Js.Array2.findIndex(player => player.score >= gameRules.winningScore) > -1
    switch hasWinner {
    | true => (playersArr->Belt.Array.concat([player]), gameState)
    | false => {
        let (curr, nums) = roll(gameState.dieRolls, ~numTimes=3, ())
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
          dieRolls: curr,
          turn: idx == 0 ? gameState.turn + 1 : gameState.turn,
        }
        (playersArr->Belt.Array.concat([newPlayerRecord]), newGameState)
      }
    }
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
      dieRolls: 0,
      turn: 0,
    }
    realFile
    ->processInput
    ->createStartingPlayerState
    ->playGame(startingGameState, rules)
    ->{
      ((playerState, gameState)) => {
        let lastPlayer =
          playerState
          ->Js.Array2.copy
          ->Js.Array2.sortInPlaceWith((player1, player2) => {
            switch player1.score - player2.score {
            | x if x < 0 => 1
            | x if x > 0 => -1
            | _ => 0
            }
          })
          ->Js.Array2.pop
        switch lastPlayer {
        | Some(player) => (player.score * gameState.dieRolls)->Js.log2("is the answer")
        | None => Js.Exn.raiseError("could not find last player")
        }
      }
    }
  } catch {
  | _ => Js.log("something went wrong")
  }
}
