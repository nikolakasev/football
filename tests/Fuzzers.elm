module Fuzzers exposing (..)

import Fuzz exposing (..)
import Random.Pcg as Random
import Shrink
import Main exposing (Settings)


settings : Fuzzer Settings
settings =
    Fuzz.custom
        --Generator a
        (Random.map4
            Settings
            --TODO randomize
            (Random.constant 40)
            (Random.int 2 8)
            --TODO randomize the following two as well
            (Random.int 1 40)
            (Random.int 1 40)
        )
        --Shrinker a
        (\{ gameDuration, numberOfPlayers, changeKeeper, changePlayer } ->
            Shrink.map Settings (Shrink.int gameDuration)
                |> Shrink.andMap (Shrink.int numberOfPlayers)
                |> Shrink.andMap (Shrink.int changeKeeper)
                |> Shrink.andMap (Shrink.int changePlayer)
        )
