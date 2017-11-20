module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)


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


type alias Substitute =
    { atMinute : Int
    , playerIn : Player
    , playerOut : Player
    , timePlayed : Int
    }


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



-- takes settings, the team and the current selection: computes the play schema and updates the time each player has played


teamPlays : Settings -> Team -> List Player -> ( List Substitute, Team )
teamPlays settings team players =
    let
        substitutes =
            computeSubstitutions settings (substituteAtMinute settings) team
    in
        ( substitutes, updatePlayTimes team substitutes )


computeSubstitutions : Settings -> List Int -> Team -> List Substitute
computeSubstitutions settings times team =
    []


updatePlayTimes : Team -> List Substitute -> Team
updatePlayTimes team substitutes =
    case substitutes of
        [] ->
            team

        h :: t ->
            updatePlayTimes (updatePlayerTime team h.playerOut h.timePlayed) t



{-
   TODO how to write a generic function that takes a list of records and two properties to search for and update?
   In this case player.name and player.totalPlayTimeInMinutes
-}


updatePlayerTime : Team -> Player -> Int -> Team
updatePlayerTime team player time =
    let
        p =
            List.filter (\p -> p.name == player.name) team
                |> List.head
    in
        case p of
            Just y ->
                { y | totalPlayTimeInMinutes = y.totalPlayTimeInMinutes + time }
                    :: List.filter (\p -> p.name /= y.name) team

            Nothing ->
                team


substituteAtMinute : Settings -> List Int
substituteAtMinute settings =
    let
        playerChangeAtMinute =
            List.range 0 (settings.gameDuration // settings.changePlayer)
                |> List.map (\t -> t * settings.changePlayer)

        keeperChangeAtMinute =
            List.range 0 (settings.gameDuration // settings.changeKeeper)
                |> List.map (\t -> t * settings.changeKeeper)
    in
        playerChangeAtMinute
            ++ keeperChangeAtMinute
            -- might have to substitue a player and the keeper at the same minute
            |> flip distinct []
            |> List.sort
            -- changing at the last minute of the game doesn't make sense
            |> List.filter (\t -> t /= settings.gameDuration)


distinct : List Int -> List Int -> List Int
distinct ints acc =
    case ints of
        [] ->
            acc

        h :: t ->
            case List.member h acc of
                True ->
                    distinct t acc

                False ->
                    distinct t (h :: acc)


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
