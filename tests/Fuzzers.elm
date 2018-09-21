module Fuzzers exposing (player, settings)

import Fuzz exposing (..)
import Main exposing (Player, Settings)
import Random.Pcg
import Random.Pcg.Char
import Random.Pcg.String
import Shrink


settings : Fuzzer Settings
settings =
    let
        --TODO randomize this as well
        gameDuration =
            40
    in
    Fuzz.custom
        --Generator a
        (Random.Pcg.map4
            Settings
            (Random.Pcg.int 1 gameDuration)
            (Random.Pcg.int 2 8)
            (Random.Pcg.int 1 gameDuration)
            (Random.Pcg.int 1 gameDuration)
        )
        --Shrinker a
        (\{ gameDuration, numberOfPlayers, changeKeeper, changePlayer } ->
            Shrink.map Settings (Shrink.int gameDuration)
                |> Shrink.andMap (Shrink.int numberOfPlayers)
                |> Shrink.andMap (Shrink.int changeKeeper)
                |> Shrink.andMap (Shrink.int changePlayer)
        )


player : Fuzzer Player
player =
    let
        sevenLetterEnglishWord =
            Random.Pcg.String.string 7 Random.Pcg.Char.english
    in
    Fuzz.custom
        (Random.Pcg.map3
            Player
            sevenLetterEnglishWord
            (Random.Pcg.int 1 Random.Pcg.maxInt)
            (Random.Pcg.int 1 Random.Pcg.maxInt)
        )
        --Shrinker a
        (\{ name, totalPlayTimeInMinutes, timesKept } ->
            Shrink.map Player (Shrink.string name)
                |> Shrink.andMap (Shrink.int totalPlayTimeInMinutes)
                |> Shrink.andMap (Shrink.int timesKept)
        )
