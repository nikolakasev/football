module Tests exposing (suite, teamPlaysWith, totalPlayTime)

import Expect exposing (equal)
import Fuzz exposing (list)
import Fuzzers exposing (..)
import Main exposing (Player, Settings, teamPlays)
import Test exposing (..)


suite : Test
suite =
    describe "The substitutes algorithm"
        [ describe "total play time"
            -- Nest as many descriptions as you like.
            [ fuzz2 Fuzzers.settings (Fuzz.list Fuzzers.player) "is the sum of time players played" <|
                \andSettings players ->
                    teamPlaysWith players andSettings
            , todo "play time for players not present remains the same"
            ]
        ]


teamPlaysWith : List Player -> Settings -> Expect.Expectation
teamPlaysWith team s =
    let
        teamHasPlayed =
            teamPlays team team s
    in
    --when a generated team doesn't match the generated settings, pass directly
    if List.length team < s.numberOfPlayers then
        Expect.pass

    else
        Expect.equal
            --total time is always the same: duration * number of players
            (s.gameDuration * s.numberOfPlayers)
            --when the team has played, they've increased their total play time
            (totalPlayTime teamHasPlayed - totalPlayTime team)


totalPlayTime : List Player -> Int
totalPlayTime players =
    List.map .totalPlayTimeInMinutes players
        |> List.foldl (+) 0
