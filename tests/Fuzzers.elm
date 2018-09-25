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
        minutesGameLasts =
            90

        minNumberOfPlayers =
            2

        maxNumberOfPlayers =
            11
    in
    Fuzz.custom
        --Generator a
        (Random.map4
            Settings
            (Random.int 1 minutesGameLasts)
            (Random.int minNumberOfPlayers maxNumberOfPlayers)
            (Random.int 1 minutesGameLasts)
            (Random.int 1 minutesGameLasts)
        )
        --Shrinker a
        (\{ gameDuration, numberOfPlayers, changeKeeper, changePlayer } ->
            Shrink.map Settings (Shrink.int minutesGameLasts)
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
