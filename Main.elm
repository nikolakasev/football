module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import List.Extra exposing (groupWhile)


type Msg
    = SetupTeam
    | PlaySchema
    | TeamConfirmedmyste
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
    { team : Team, present : List Player, playerToAdd : String, state : State, settings : Settings }


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
    [ { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 }
    , { name = "Elias", totalPlayTimeInMinutes = 89, timesKept = 3 }
    , { name = "Rein", totalPlayTimeInMinutes = 100, timesKept = 4 }
    , { name = "Mats", totalPlayTimeInMinutes = 12, timesKept = 2 }
    , { name = "Kjeld", totalPlayTimeInMinutes = 43, timesKept = 1 }
    , { name = "Jeroen", totalPlayTimeInMinutes = 54, timesKept = 0 }
    , { name = "Rafael", totalPlayTimeInMinutes = 45, timesKept = 0 }
    , { name = "Kaan", totalPlayTimeInMinutes = 23, timesKept = 1 }
    ]


newTeam : Team
newTeam =
    [ { name = "Kaya", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Elias", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Rein", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Mats", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Kjeld", totalPlayTimeInMinutes = 0, timesKept = 0 }
    , { name = "Jeroen", totalPlayTimeInMinutes = 0, timesKept = 0 }
    ]


mySettings : Settings
mySettings =
    { gameDuration = 40, numberOfPlayers = 6, changeKeeper = 10, changePlayer = 5 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { team = myTeam, present = [], playerToAdd = "", state = Menu, settings = mySettings }
        , update = update
        }


view : Model -> Html Msg
view model =
    case model.state of
        Menu ->
            showMainMenu

        Players ->
            showPlayers model.team

        Schema ->
            showPlaySchema model.team

        GameUnderway ->
            showGameUnderway model.team


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetupTeam ->
            { model | state = Players }

        PlaySchema ->
            { model | state = Schema }

        PlayerAdded ->
            { model
                | team = addPlayerToTeam { name = model.playerToAdd, totalPlayTimeInMinutes = 0, timesKept = 0 } model.team
            }

        PlayerNamed name ->
            { model | playerToAdd = name }

        PlayerRemoved name ->
            { model | team = List.filter (\p -> p.name /= name) model.team }

        Play ->
            { model | state = GameUnderway }

        GameEnded ->
            { model | state = Players }

        GoToMain ->
            { model | state = Menu }

        PlayerPresenseChanged name ->
            { model | present = updatePlayerPresense name model.team model.present }

        _ ->
            model


showMainMenu : Html Msg
showMainMenu =
    div []
        [ Html.button [ onClick SetupTeam ] [ text "My Team" ]
        , Html.br [] []
        , Html.button [ onClick PlaySchema ] [ text "Play Schema" ]
        ]


showMyTeam : Html Msg
showMyTeam =
    div []
        [ div [] [ text "My Team" ]
        , div [] [ text "here come the players" ]
        ]


showPlayers : Team -> Html Msg
showPlayers players =
    div []
        --render each player with most active in top
        ((List.sortBy .totalPlayTimeInMinutes players
            |> List.reverse
            |> List.map playerToHtml
         )
            ++ [ input [ placeholder "Player name", onInput PlayerNamed ] []
               , button [ onClick PlayerAdded ] [ text "Add" ]
               , br [] []
               , button [ onClick PlaySchema ] [ text "Play Schema" ]
               ]
        )


showPlaySchema : List Player -> Html Msg
showPlaySchema players =
    div []
        [ text "Present today:"
        , br [] []
        , presentToday players
        , cancel
        , button [ onClick Play ] [ text "Play!" ]
        ]


showGameUnderway : Team -> Html Msg
showGameUnderway team =
    div []
        [ text "Game is underway"
        , button [ onClick GameEnded ] [ text "End Game" ]
        ]


playerToHtml : Player -> Html Msg
playerToHtml player =
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


teamPlays : Settings -> List Player -> List Player
teamPlays settings present =
    let
        substitutes =
            computePlayJournal settings.numberOfPlayers (substituteAtMinute settings) present

        lastJournal =
            List.head substitutes |> Maybe.withDefault defaultJournal

        minutesPlayedAfterLastSubstitution =
            settings.gameDuration - lastJournal.atMinute

        team =
            updatePlayersTime (lastJournal.keeper :: lastJournal.playing) minutesPlayedAfterLastSubstitution
                ++ lastJournal.substitutes
    in
        team


p : List PlayJournal
p =
    [ { atMinute = 35, keeper = { name = "Mats", totalPlayTimeInMinutes = 47, timesKept = 6 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 58, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 78, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 80, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 89, timesKept = 0 }, { name = "Kaya", totalPlayTimeInMinutes = 105, timesKept = 0 } ], substitutes = [ { name = "Rein", totalPlayTimeInMinutes = 110, timesKept = 4 }, { name = "Elias", totalPlayTimeInMinutes = 109, timesKept = 3 } ] }
    , { atMinute = 30, keeper = { name = "Mats", totalPlayTimeInMinutes = 42, timesKept = 6 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 53, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 73, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 75, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 84, timesKept = 0 }, { name = "Rein", totalPlayTimeInMinutes = 105, timesKept = 4 } ], substitutes = [ { name = "Elias", totalPlayTimeInMinutes = 109, timesKept = 3 }, { name = "Kaya", totalPlayTimeInMinutes = 105, timesKept = 0 } ] }
    , { atMinute = 25, keeper = { name = "Mats", totalPlayTimeInMinutes = 37, timesKept = 5 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 48, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 68, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 70, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 79, timesKept = 0 }, { name = "Elias", totalPlayTimeInMinutes = 104, timesKept = 3 } ], substitutes = [ { name = "Rein", totalPlayTimeInMinutes = 105, timesKept = 4 }, { name = "Kaya", totalPlayTimeInMinutes = 105, timesKept = 0 } ] }
    , { atMinute = 20, keeper = { name = "Mats", totalPlayTimeInMinutes = 32, timesKept = 5 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 43, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 63, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 65, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 74, timesKept = 0 }, { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 } ], substitutes = [ { name = "Rein", totalPlayTimeInMinutes = 105, timesKept = 4 }, { name = "Elias", totalPlayTimeInMinutes = 104, timesKept = 3 } ] }
    , { atMinute = 15, keeper = { name = "Mats", totalPlayTimeInMinutes = 27, timesKept = 4 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 38, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 58, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 60, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 69, timesKept = 0 }, { name = "Rein", totalPlayTimeInMinutes = 100, timesKept = 4 } ], substitutes = [ { name = "Elias", totalPlayTimeInMinutes = 104, timesKept = 3 }, { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 } ] }
    , { atMinute = 10, keeper = { name = "Mats", totalPlayTimeInMinutes = 22, timesKept = 4 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 33, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 53, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 55, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 64, timesKept = 0 }, { name = "Elias", totalPlayTimeInMinutes = 99, timesKept = 3 } ], substitutes = [ { name = "Rein", totalPlayTimeInMinutes = 100, timesKept = 4 }, { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 } ] }
    , { atMinute = 5, keeper = { name = "Mats", totalPlayTimeInMinutes = 17, timesKept = 3 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 28, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 48, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 50, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 59, timesKept = 0 }, { name = "Elias", totalPlayTimeInMinutes = 94, timesKept = 3 } ], substitutes = [ { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 }, { name = "Rein", totalPlayTimeInMinutes = 100, timesKept = 4 } ] }
    , { atMinute = 0, keeper = { name = "Mats", totalPlayTimeInMinutes = 12, timesKept = 3 }, playing = [ { name = "Kaan", totalPlayTimeInMinutes = 23, timesKept = 1 }, { name = "Kjeld", totalPlayTimeInMinutes = 43, timesKept = 1 }, { name = "Rafael", totalPlayTimeInMinutes = 45, timesKept = 0 }, { name = "Jeroen", totalPlayTimeInMinutes = 54, timesKept = 0 }, { name = "Elias", totalPlayTimeInMinutes = 89, timesKept = 3 } ], substitutes = [ { name = "Rein", totalPlayTimeInMinutes = 100, timesKept = 4 }, { name = "Kaya", totalPlayTimeInMinutes = 100, timesKept = 0 } ] }
    ]


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
                    List.sortBy .totalPlayTimeInMinutes present
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


presentToday : List Player -> Html Msg
presentToday players =
    div []
        (List.map
            (\p -> checkbox (PlayerPresenseChanged p.name) p.name)
            players
        )


checkbox : Msg -> String -> Html Msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


cancel : Html Msg
cancel =
    -- TODO: is this the best way to navigate?
    button [ onClick GoToMain ] [ text "Cancel" ]
