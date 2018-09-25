module Fuzzers exposing (player, settings)

import Fuzz exposing (..)
import Main exposing (Player, Settings)
import Random
import Random.Char
import Random.Int
import Random.String
import Shrink


settings : Fuzzer Settings
settings =
    let
        duration =
            90
    in
    Fuzz.custom
        --Generator a
        (Random.map4
            Settings
            (Random.int 1 duration)
            (Random.int 2 8)
            (Random.int 1 duration)
            (Random.int 1 duration)
        )
        --Shrinker a
        (\{ gameDuration, numberOfPlayers, changeKeeper, changePlayer } ->
            Shrink.map Settings (Shrink.int duration)
                |> Shrink.andMap (Shrink.int numberOfPlayers)
                |> Shrink.andMap (Shrink.int changeKeeper)
                |> Shrink.andMap (Shrink.int changePlayer)
        )


player : Fuzzer Player
player =
    let
        sevenLetterEnglishWord =
            Random.String.string 7 Random.Char.english
    in
    Fuzz.custom
        (Random.map3
            Player
            sevenLetterEnglishWord
            Random.Int.positiveInt
            Random.Int.positiveInt
        )
        --Shrinker a
        (\{ name, totalPlayTimeInMinutes, timesKept } ->
            Shrink.map Player (Shrink.string name)
                |> Shrink.andMap (Shrink.int totalPlayTimeInMinutes)
                |> Shrink.andMap (Shrink.int timesKept)
        )
