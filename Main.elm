module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder)


type Msg
    = MyTeam
    | PlaySchema
    | Settings
    | TeamConfirmed
    | PlayerNamed String
    | PlayerAdded
    | PlayerRemoved String


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


type alias Team =
    List Player


type alias Player =
    { name : String
    , totalPlayTimeInMinutes : Int
    , timesKept : Int
    }


type alias Model =
    { team : Team, playerToAdd : String }


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { view = view
        , model = { team = myTeam, playerToAdd = "" }
        , update = update
        }


view : Model -> Html Msg
view model =
    showPlayers model.team


update : Msg -> Model -> Model
update msg model =
    case msg of
        PlayerAdded ->
            { model
                | team = addPlayerToTeam { name = model.playerToAdd, totalPlayTimeInMinutes = 0, timesKept = 0 } model.team
            }

        PlayerNamed name ->
            { model | playerToAdd = name }

        PlayerRemoved name ->
            { model | team = List.filter (\p -> p.name /= name) model.team }

        _ ->
            model


showMainMenu : Html Msg
showMainMenu =
    div []
        [ Html.button [ onClick MyTeam ] [ text "My Team" ]
        , Html.br [] []
        , Html.button [ onClick PlaySchema ] [ text "Play Schema" ]
        , Html.br [] []
        , Html.button [ onClick Settings ] [ text "Settings" ]
        ]


showMyTeam : Html Msg
showMyTeam =
    div []
        [ div [] [ text "My Team" ]
        , div [] [ text "here come the players" ]
        ]


showPlayers : List Player -> Html Msg
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
