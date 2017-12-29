module Main exposing (main)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List.Extra exposing (groupWhile)


type Msg
    = SetupTeam
    | PlaySchema
    | PlayerNamed String
    | PlayerAdded
    | PlayerRemoved String
      -- this gets emitted from the checkbox
    | PlayerPresenseChanged String
    | Play
    | GameEnded
    | GoToMain


type alias Team =
    List Player


type alias Player =
    { name : String
    , totalPlayTimeInMinutes : Int
    , timesKept : Int
    }


defaultPlayer : Player
defaultPlayer =
    { name = "John Doe", totalPlayTimeInMinutes = 707, timesKept = 707 }


defaultJournal : PlayJournal
defaultJournal =
    { atMinute = 707, keeper = defaultPlayer, playing = [ defaultPlayer ], substitutes = [ defaultPlayer ] }


type alias PlayJournal =
    { atMinute : Int
    , keeper : Player
    , playing : List Player
    , substitutes : List Player
    }


type alias Substitute =
    { atMinute : Int, substituteWhom : SubstituteType }


type SubstituteType
    = Playr
    | Keeper
    | Both


type alias Model =
    { team : Team, present : List Player, journal : List PlayJournal, playerToAdd : String, state : State, settings : Settings }


type State
    = Menu
    | Players
    | Schema
    | GameUnderway


type alias Settings =
    { --in minutes
      gameDuration : Int
    , numberOfPlayers : Int

    --how often to change the keeper
    , changeKeeper : Int

    --how ofter to change a player
    , changePlayer : Int
    }


myTeam : Team
myTeam =
    [ { name = "Kaya", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Elias", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Rein", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Mats", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Kjeld", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Jeroen", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Rafael", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Kaan", totalPlayTimeInMinutes = 0, timesKept = 0 }
    ]


mySettings : Settings
mySettings =
    { gameDuration = 40, numberOfPlayers = 6, changeKeeper = 10, changePlayer = 5 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { team = myTeam, present = [], journal = [], playerToAdd = "", state = Menu, settings = mySettings }
        , update = update
        }


view : Model -> Html Msg
view model =
    case model.state of
        Menu ->
            mainMenuView

        Players ->
            playersView model.team

        Schema ->
            playersPresentView model.team model.present model.settings.numberOfPlayers

        GameUnderway ->
            gameUnderwayView model.journal


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetupTeam ->
            { model | state = Players }

        PlaySchema ->
            { model | state = Schema, present = model.team }

        PlayerAdded ->
            { model
                | team = addPlayerToTeam { name = model.playerToAdd, totalPlayTimeInMinutes = 0, timesKept = 0 } model.team
            }

        PlayerNamed name ->
            { model | playerToAdd = name }

        PlayerRemoved name ->
            { model | team = List.filter (\p -> p.name /= name) model.team }

        Play ->
            let
                settings =
                    model.settings

                j =
                    computePlayJournal
                        settings.numberOfPlayers
                        (substituteAtMinute settings)
                        model.present
            in
                { model | state = GameUnderway, journal = j }

        GameEnded ->
            { model
                | state = Players
                , team = updateTeamPlayTime model.team model.journal model.settings.gameDuration
                , present = []
            }

        GoToMain ->
            { model | state = Menu }

        PlayerPresenseChanged name ->
            { model | present = updatePlayerPresense name model.team model.present }


mainMenuView : Html Msg
mainMenuView =
    div []
        [ Html.button [ onClick SetupTeam ] [ text "My Team" ]
        , Html.br [] []
        , Html.button [ onClick PlaySchema ] [ text "Play Schema" ]
        ]


playersView : Team -> Html Msg
playersView players =
    div []
        --render each player with most active in top
        ((List.sortBy .totalPlayTimeInMinutes players
            |> List.reverse
            |> List.map playerView
         )
            ++ [ input [ placeholder "Player name", onInput PlayerNamed ] []
               , button [ onClick PlayerAdded ] [ text "Add" ]
               , br [] []
               , button [ onClick PlaySchema ] [ text "Play Schema" ]
               ]
        )


playersPresentView : Team -> List Player -> Int -> Html Msg
playersPresentView team present numberOfPlayersNeeded =
    div []
        [ text "Present today:"
        , br [] []

        --start from the complete team, but allow to select who is present
        , presentTodayView team
        , cancel
        , button [ onClick Play, disabled (List.length present < numberOfPlayersNeeded) ] [ text "Play!" ]
        ]


presentTodayView : List Player -> Html Msg
presentTodayView players =
    div []
        (List.map
            (\p -> checkbox (PlayerPresenseChanged p.name) p.name)
            players
        )


checkbox : Msg -> String -> Html Msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", checked True, onClick msg ] []
        , text name
        ]


gameUnderwayView : List PlayJournal -> Html Msg
gameUnderwayView journal =
    div []
        [ text "Game is underway"
        , br [] []
        , playSchemaView journal
        , br [] []
        , button [ onClick GameEnded ] [ text "End Game" ]
        ]


playSchemaView : List PlayJournal -> Html Msg
playSchemaView journal =
    let
        journalSorted =
            List.sortBy .atMinute journal

        pairs =
            journalSorted
                --group in pairs and overlap, for easier mapping of substitutions
                |> List.Extra.groupsOfWithStep 2 1

        toTuple =
            \pair ->
                case pair of
                    comesOut :: comesIn :: [] ->
                        substitutionView comesIn comesOut

                    _ ->
                        []
    in
        div []
            ((List.head journalSorted
                |> Maybe.withDefault defaultJournal
                |> beginPlayView
             )
                :: br [] []
                :: (List.map toTuple pairs
                        |> List.concat
                   )
            )


beginPlayView : PlayJournal -> Html msg
beginPlayView journalAtTheBeginning =
    text
        ("Keeper: "
            ++ journalAtTheBeginning.keeper.name
            ++ if List.length journalAtTheBeginning.substitutes > 0 then
                ", substitutes: "
                    ++ (List.map .name journalAtTheBeginning.substitutes
                            |> String.join ", "
                       )
               else
                ""
        )


substitutionView : PlayJournal -> PlayJournal -> List (Html msg)
substitutionView playersIn playersOut =
    (List.Extra.zip playersIn.substitutes playersOut.substitutes
        |> List.map
            (\( out, inn ) ->
                --same player remains as a substitution in two consecutive moments to substitute
                if inn.name /= out.name then
                    [ text (toString playersIn.atMinute ++ " min.: " ++ inn.name ++ "⇄" ++ out.name), br [] [] ]
                else
                    []
            )
        |> List.concat
    )
        ++ --show if there was a change of keepers as well
           if playersIn.keeper.name /= playersOut.keeper.name then
            [ text (toString playersIn.atMinute ++ " min. (k): " ++ playersIn.keeper.name ++ "⇄" ++ playersOut.keeper.name), br [] [] ]
           else
            []


playerView : Player -> Html Msg
playerView player =
    div []
        [ text
            (String.join ", "
                [ player.name
                , minutesToString player.totalPlayTimeInMinutes
                , keptToString player.timesKept
                ]
            )
        , button [ onClick (PlayerRemoved player.name) ] [ text "Remove" ]
        ]


minutesToString : Int -> String
minutesToString minutes =
    if minutes < 0 then
        ""
    else if minutes == 0 then
        "0 min."
    else if minutes < 60 then
        toString minutes ++ " min."
    else if minutes == 60 then
        "1 h."
    else
        let
            hours =
                minutes // 60
        in
            toString hours ++ " h. " ++ toString (minutes - hours * 60) ++ " min."


keptToString : Int -> String
keptToString times =
    case times of
        0 ->
            "not yet"

        1 ->
            "kept once"

        2 ->
            "kept twice"

        _ ->
            "kept " ++ toString times ++ " times"


addPlayerToTeam : Player -> Team -> Team
addPlayerToTeam player team =
    case List.any (\p -> p.name == player.name) team of
        True ->
            team

        False ->
            player :: team


updatePlayerPresense : String -> Team -> List Player -> List Player
updatePlayerPresense playerName team present =
    case List.any (\p -> p.name == playerName) present of
        --remove if already present
        True ->
            List.filter (\p -> p.name /= playerName) present

        --add if not present
        False ->
            present ++ List.filter (\p -> p.name == playerName) team


computePlayJournal : Int -> List Substitute -> List Player -> List PlayJournal
computePlayJournal numberOfPlayers times present =
    let
        choose =
            \n t p m acc ->
                case t of
                    [] ->
                        acc

                    head :: tail ->
                        case head.atMinute of
                            --game begins, must give a role to all present players
                            0 ->
                                let
                                    ( players, substitutes ) =
                                        choosePlayersAndSubstitutes p n

                                    keeper =
                                        chooseKeeper players

                                    playersWithoutKeeper =
                                        List.drop 1 players

                                    journalEntry =
                                        { atMinute = 0, keeper = keeper, playing = playersWithoutKeeper, substitutes = substitutes }
                                in
                                    choose n tail p 0 (journalEntry :: acc)

                            --game underway
                            _ ->
                                case head.substituteWhom of
                                    Playr ->
                                        let
                                            currentSituation =
                                                List.head acc
                                                    |> Maybe.withDefault defaultJournal

                                            --update play time
                                            timePlayedSoFar =
                                                (head.atMinute - m)

                                            keeper =
                                                (updatePlayerTime currentSituation.keeper timePlayedSoFar)

                                            playing =
                                                updatePlayersTime currentSituation.playing timePlayedSoFar

                                            ( players, substitutes ) =
                                                choosePlayersAndSubstitutes (playing ++ currentSituation.substitutes) (n - 1)

                                            journalEntry =
                                                { atMinute = head.atMinute, keeper = keeper, playing = players, substitutes = substitutes }
                                        in
                                            choose n tail p head.atMinute (journalEntry :: acc)

                                    Keeper ->
                                        let
                                            currentSituation =
                                                List.head acc
                                                    |> Maybe.withDefault defaultJournal

                                            --update play time
                                            timePlayedSoFar =
                                                (head.atMinute - m)

                                            currentKeeper =
                                                (updatePlayerTime currentSituation.keeper timePlayedSoFar)

                                            playing =
                                                updatePlayersTime currentSituation.playing timePlayedSoFar

                                            ( players, substitutes ) =
                                                choosePlayersAndSubstitutes ([ currentKeeper ] ++ playing ++ currentSituation.substitutes) n

                                            keeper =
                                                chooseKeeper players

                                            playersWithoutKeeper =
                                                List.drop 1 players

                                            journalEntry =
                                                { atMinute = head.atMinute, keeper = keeper, playing = playersWithoutKeeper, substitutes = substitutes }
                                        in
                                            choose n tail p head.atMinute (journalEntry :: acc)

                                    Both ->
                                        let
                                            currentSituation =
                                                List.head acc
                                                    |> Maybe.withDefault defaultJournal

                                            --update play time
                                            timePlayedSoFar =
                                                (head.atMinute - m)

                                            playing =
                                                updatePlayersTime (currentSituation.keeper :: currentSituation.playing) timePlayedSoFar

                                            ( players, substitutes ) =
                                                choosePlayersAndSubstitutes (playing ++ currentSituation.substitutes) n

                                            keeper =
                                                chooseKeeper players

                                            playersWithoutKeeper =
                                                List.drop 1 players

                                            journalEntry =
                                                { atMinute = head.atMinute, keeper = keeper, playing = playersWithoutKeeper, substitutes = substitutes }
                                        in
                                            choose n tail p head.atMinute (journalEntry :: acc)
    in
        choose numberOfPlayers times present 0 []


updateTeamPlayTime : Team -> List PlayJournal -> Int -> Team
updateTeamPlayTime team journal gameDuration =
    let
        lastJournal =
            List.Extra.maximumBy .atMinute journal
                |> Maybe.withDefault defaultJournal

        minutesPlayedAfterLastSubstitution =
            gameDuration - lastJournal.atMinute

        present =
            updatePlayersTime (lastJournal.keeper :: lastJournal.playing) minutesPlayedAfterLastSubstitution
                ++ lastJournal.substitutes

        notPresent =
            List.Extra.filterNot (playerPresent present) team
    in
        present ++ notPresent


playerPresent : List Player -> Player -> Bool
playerPresent present player =
    List.any (\p -> p.name == player.name) present


choosePlayersAndSubstitutes : List Player -> Int -> ( List Player, List Player )
choosePlayersAndSubstitutes present numberOfPlayers =
    case present of
        [] ->
            ( [], [] )

        _ ->
            let
                playersExtra =
                    List.length present - numberOfPlayers

                rankedForPlayTimeDescending =
                    --first sort
                    List.sortBy .totalPlayTimeInMinutes present
                        --then group by if players played the same amount of time
                        |> List.Extra.groupWhile (\x y -> x.totalPlayTimeInMinutes == y.totalPlayTimeInMinutes)
                        |> List.map
                            (List.sortBy .timesKept)
                        --turn List List Player to List Player
                        |> List.concat
                        --reverse, so the keeper to be chosen is the head
                        |> List.reverse

                substitutes =
                    rankedForPlayTimeDescending
                        |> List.take playersExtra

                playing =
                    rankedForPlayTimeDescending
                        |> List.drop playersExtra
                        |> List.reverse
            in
                ( playing, substitutes )


chooseKeeper : List Player -> Player
chooseKeeper players =
    let
        keeper =
            List.head players
                |> Maybe.withDefault defaultPlayer
    in
        { keeper | timesKept = keeper.timesKept + 1 }


updatePlayersTime : List Player -> Int -> List Player
updatePlayersTime players time =
    let
        update =
            \players time acc ->
                case players of
                    [] ->
                        acc

                    head :: tail ->
                        update tail time ({ head | totalPlayTimeInMinutes = head.totalPlayTimeInMinutes + time } :: acc)
    in
        update players time []


updatePlayerTime : Player -> Int -> Player
updatePlayerTime player time =
    { player | totalPlayTimeInMinutes = player.totalPlayTimeInMinutes + time }


substituteAtMinute : Settings -> List Substitute
substituteAtMinute settings =
    let
        playerChangeAtMinute =
            List.range 0 (settings.gameDuration // settings.changePlayer)
                |> List.map (\t -> { atMinute = t * settings.changePlayer, substituteWhom = Playr })

        keeperChangeAtMinute =
            List.range 0 (settings.gameDuration // settings.changeKeeper)
                |> List.map (\t -> { atMinute = t * settings.changeKeeper, substituteWhom = Keeper })
    in
        playerChangeAtMinute
            ++ keeperChangeAtMinute
            |> List.sortBy .atMinute
            |> List.Extra.groupWhile (\x y -> x.atMinute == y.atMinute)
            |> List.map playerKeeperOrBoth
            --remove any Nothing occurrences
            |> List.filterMap identity
            --remove occurences that are at the exact end of the game
            |> List.filter (\s -> s.atMinute /= settings.gameDuration)


playerKeeperOrBoth : List Substitute -> Maybe Substitute
playerKeeperOrBoth tuples =
    case tuples of
        [] ->
            Nothing

        head :: [] ->
            Just { atMinute = head.atMinute, substituteWhom = head.substituteWhom }

        head :: tail ->
            Just { atMinute = head.atMinute, substituteWhom = Both }


cancel : Html Msg
cancel =
    -- TODO: is this the best way to navigate?
    button [ onClick GoToMain ] [ text "Cancel" ]
