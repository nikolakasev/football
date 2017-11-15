module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)


type Msg
    = SetupTeam
    | PlaySchema
    | TeamConfirmed
    | PlayerNamed String
    | PlayerAdded
    | PlayerRemoved String
    | Play
    | GameEnded


type alias Team =
    List Player


type alias Player =
    { name : String
    , totalPlayTimeInMinutes : Int
    , timesKept : Int
    }


type alias Substitute =
    { atMinute : Int
    , playerIn : Player
    , playerOut : Maybe Player
    , isKeeper : Bool
    }


type alias Model =
    { team : Team, playerToAdd : String, state : State, settings : Settings }


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


mySettings : Settings
mySettings =
    { gameDuration = 40, numberOfPlayers = 6, changeKeeper = 10, changePlayer = 5 }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { team = myTeam, playerToAdd = "", state = Menu, settings = mySettings }
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
            ++ [ Html.input [ placeholder "Player name", onInput PlayerNamed ] []
               , Html.button [ onClick PlayerAdded ] [ text "Add" ]
               , Html.br [] []
               , Html.button [ onClick PlaySchema ] [ text "Play Schema" ]
               ]
        )


showPlaySchema : Team -> Html Msg
showPlaySchema team =
    div []
        [ text "Present today:"
        , Html.br [] []
        , Html.button [ onClick Play ] [ text "Play!" ]
        ]


showGameUnderway : Team -> Html Msg
showGameUnderway team =
    div []
        [ text "Game is underway"
        , Html.button [ onClick GameEnded ] [ text "End Game" ]
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
        , Html.button [ onClick (PlayerRemoved player.name) ] [ text "Remove" ]
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



-- takes settings, the team and the current selection: computes the play schema and updates the time each player has played


teamPlays : Settings -> Team -> List Player -> ( List Substitute, Team )
teamPlays settings team players =
    ( [], [] )


keeperPlays : Settings -> List Player -> List Substitute
keeperPlays settings players =
    let
        keepersNeeded =
            settings.gameDuration // settings.changeKeeper
    in
        List.sortBy .timesKept players
            |> List.take keepersNeeded
            |> List.indexedMap (\i player -> ( i * settings.changeKeeper, player ))
            |> flip keepers []


keepers : List ( Int, Player ) -> List Substitute -> List Substitute
keepers players substitutes =
    case players of
        [] ->
            substitutes

        h :: t ->
            case substitutes of
                [] ->
                    keepers t ({ atMinute = Tuple.first h, playerIn = Tuple.second h, playerOut = Nothing, isKeeper = True } :: substitutes)

                s :: _ ->
                    keepers t ({ atMinute = Tuple.first h, playerIn = Tuple.second h, playerOut = Just s.playerIn, isKeeper = True } :: substitutes)
