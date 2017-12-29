module Fuzzers exposing (position)

import Fuzz exposing (..)
import Random.Pcg as Random
import Shrink


type alias Position =
    { x : Int, y : Int }


position : Fuzzer Position
position =
    Fuzz.custom
        --Generator a
        (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
        --Shrinker a
        (\{ x, y } -> Shrink.map Position (Shrink.int x) |> Shrink.andMap (Shrink.int y))
