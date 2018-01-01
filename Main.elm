module Main exposing (main, teamPlays, Player, Settings)

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
                    computePlaySchema
                        (substituteAtMinute settings)
                        settings.numberOfPlayers
                        model.present
            in
                { model | state = GameUnderway, journal = j }

        GameEnded ->
            { model
                | state = Players
                , team = teamPlays model.team model.present model.settings
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
                |> beginPlayView
             )
                :: br [] []
                :: (List.map toTuple pairs
                        |> List.concat
                   )
            )


beginPlayView : Maybe PlayJournal -> Html msg
beginPlayView journalAtTheBeginning =
    case journalAtTheBeginning of
        Just playJournal ->
            text
                ("Keeper: "
                    ++ playJournal.keeper.name
                    --show the substitutes if any
                    ++ if List.length playJournal.substitutes > 0 then
                        ", substitutes: "
                            ++ (List.map .name playJournal.substitutes
                                    |> String.join ", "
                               )
                       else
                        ""
                )

        Nothing ->
            text "beginPlayView: No journal entry provided."


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


computePlaySchema : List Substitute -> Int -> List Player -> List PlayJournal
computePlaySchema times numberOfPlayers present =
    let
        compute =
            \t n p acc ->
                case t of
                    [] ->
                        acc

                    head :: tail ->
                        case head.atMinute of
                            --game begins, must give a role to all present players
                            0 ->
                                let
                                    ( keeper, players, substitutes ) =
                                        chooseKeeperPlayersAndSubstitutes p n
                                in
                                    case keeper of
                                        Nothing ->
                                            compute tail n p []

                                        Just player ->
                                            compute tail n p ({ atMinute = 0, keeper = player, playing = players, substitutes = substitutes } :: [])

                            --game underway
                            _ ->
                                let
                                    next =
                                        computeNextJournalEntry head acc n
                                in
                                    case next of
                                        Nothing ->
                                            compute tail n p acc

                                        Just journal ->
                                            compute tail n p (journal :: acc)
    in
        compute times numberOfPlayers present []


computeNextJournalEntry : Substitute -> List PlayJournal -> Int -> Maybe PlayJournal
computeNextJournalEntry substitute journalSoFar numberOfPlayers =
    --given a list of journal entries, compute the next one
    Nothing


updateTeamPlayTime : Team -> List PlayJournal -> Int -> Team
updateTeamPlayTime team journal gameDuration =
    let
        latestJournal =
            List.Extra.maximumBy .atMinute journal

        calculate =
            \journal team duration ->
                let
                    minutesPlayedAfterLastSubstitution =
                        duration - journal.atMinute

                    present =
                        updatePlayersTime (journal.keeper :: journal.playing) minutesPlayedAfterLastSubstitution
                            ++ journal.substitutes

                    notPresent =
                        List.Extra.filterNot (playerPresent present) team
                in
                    present ++ notPresent
    in
        case latestJournal of
            --a journal is mandatory to calculate the new player play times
            Nothing ->
                team

            Just journal ->
                calculate journal team gameDuration


teamPlays : Team -> List Player -> Settings -> Team
teamPlays team present settings =
    let
        substituteThen =
            substituteAtMinute settings

        journal =
            computePlaySchema substituteThen settings.numberOfPlayers present
    in
        updateTeamPlayTime team journal settings.gameDuration


playerPresent : List Player -> Player -> Bool
playerPresent present player =
    List.any (\p -> p.name == player.name) present


chooseKeeperPlayersAndSubstitutes : List Player -> Int -> ( Maybe Player, List Player, List Player )
chooseKeeperPlayersAndSubstitutes present numberOfPlayers =
    case present of
        [] ->
            ( Nothing, [], [] )

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

                keeper =
                    chooseKeeper playing
            in
                ( keeper, List.drop 1 playing, substitutes )


chooseKeeper : List Player -> Maybe Player
chooseKeeper players =
    case players of
        [] ->
            Nothing

        player :: _ ->
            Just { player | timesKept = player.timesKept + 1 }


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
