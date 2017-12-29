module Tests exposing (suite)

import Main exposing (Player, Settings, teamPlays)
import Test exposing (..)
import Expect exposing (equal)
import Fuzzers exposing (..)


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
                \_ -> teamPlaysWith testSettings
            , todo "play time for players not present remains the same"
            , fuzz Fuzzers.settings "calculates correctly for fuzzed settings" <|
                -- This anonymous function will be run 100 times, each time with
                -- randomly-generated game settings
                \s ->
                    teamPlaysWith s
            ]
        ]


teamPlaysWith : Settings -> Expect.Expectation
teamPlaysWith s =
    let
        teamHasPlayed =
            teamPlays testTeam testTeam s
    in
        Expect.equal
            --total time is always the same: duration * number of players
            (s.gameDuration * s.numberOfPlayers)
            --when the team has played, they've increased their total play time
            (totalPlayTime teamHasPlayed - totalPlayTime testTeam)
