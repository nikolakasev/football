module Tests exposing (suite)

import Main exposing (Player, Settings, teamPlays)
import Test exposing (..)
import Expect exposing (equal)
import Fuzzers exposing (position)
import Fuzz exposing (list)


--import Fuzz exposing (string)


testTeam : List Player
testTeam =
    [ { name = "Kaya", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Elias", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Rein", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Mats", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Kjeld", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Jeroen", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Rafael", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Kaan", totalPlayTimeInMinutes = 0, timesKept = 0 }
    ]


testSettings : Settings
testSettings =
    { gameDuration = 40, numberOfPlayers = 6, changeKeeper = 10, changePlayer = 5 }


totalPlayTime : List Player -> Int
totalPlayTime players =
    List.map .totalPlayTimeInMinutes players
        |> List.foldl (+) 0


suite : Test
suite =
    describe "The substitutes algorithm"
        [ describe "total play time"
            -- Nest as many descriptions as you like.
            [ test "is the sum of time players played" <|
                \_ ->
                    let
                        teamHasPlayed =
                            teamPlays testTeam testTeam testSettings
                    in
                        Expect.equal
                            --total time is always the same: duration * number of players
                            (testSettings.gameDuration * testSettings.numberOfPlayers)
                            --when the team has played, they've increased their total play time
                            (totalPlayTime teamHasPlayed - totalPlayTime testTeam)
            , todo "play time for players not present remains the same"
            , fuzz (list Fuzzers.position) "List.length should always be positive" <|
                -- This anonymous function will be run 100 times, each time with a
                -- randomly-generated fuzzList value.
                \fuzzList ->
                    fuzzList
                        |> List.length
                        |> Expect.atLeast 0
            ]
        ]
