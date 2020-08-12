module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, table, tr, td, h2, br, input, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder)
import Debug exposing (toString)
import Random

-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Actor =
    { id : Int
    , name : String
    , cc : String
    , wc : String
    , numBeersWanted : Float
    , numPizzasWanted : Float
    , hiLo : Float
    , hoursToWork : Float
    , maxLeisureTime : Int
    }


type alias Prices =
    { pizza : Float
    , beer : Float
    }


type alias Model =
    { actors : List Actor
    , prices : Prices
    , currentPane : String
    , tempCc : String
    , tempWc : String
    , tempName : String
    , tempHiLo : Float
    }


newPriceVector : Prices
newPriceVector =
    Prices 10.0 10.0


init : Model
init =
    Model [] newPriceVector "" "" "" "" 0.0


newActor : Int -> String -> String -> String -> Int -> Actor
newActor i name cc wc mlt =
    Actor i name cc wc 0.0 0.0 0.0 0.0 mlt


-- UPDATE


type Msg
    = AddActor Actor
    | ShowAddActorPane
    | ShowEditActorPane
    | ShowCouncilsPane
    | ShowDebugPane
    | ShowIterationPane
    | SelectCc String
    | SelectWc String
    | EnterName String

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddActor a ->
            { model | actors = a :: model.actors
                    , currentPane = "ShowCouncils" }

        SelectCc v ->
            { model | tempCc = v }

        SelectWc v ->
            { model | tempWc = v }

        EnterName s ->
            { model | tempName = s }

        ShowAddActorPane ->
            { model | currentPane = "AddActor" }

        ShowEditActorPane ->
            { model | currentPane = "EditActor" }

        ShowCouncilsPane ->
            { model | currentPane = "ShowCouncils" }

        ShowDebugPane ->
            { model | currentPane = "ShowDebugPane" }

        ShowIterationPane ->
            { model | currentPane = "Iterate" }


-- VIEW


viewCouncils : Model -> Html Msg
viewCouncils model =
    let
        pizzas =
            List.filter (\c -> c.wc == "pizza") model.actors |> List.map (\x -> x.name)

        beers =
            List.filter (\c -> c.wc == "beer") model.actors |> List.map (\x -> x.name)

        ones =
            List.filter (\c -> c.cc == "1") model.actors |> List.map (\x -> x.name)

        twos =
            List.filter (\c -> c.cc == "2") model.actors |> List.map (\x -> x.name)

        threes =
            List.filter (\c -> c.cc == "3") model.actors |> List.map (\x -> x.name)
    in
        div []
            [ p [] [ text "Workers Council - Pizza:" ]
            , p [] [ text (toString pizzas) ]
            , p [] [ text "Workers Council - Beer:" ]
            , p [] [ text (toString beers) ]
            , p [] [ text "==========================================" ]
            , p [] [ text "Consumers Council - One:" ]
            , p [] [ text (toString ones) ]
            , p [] [ text "Consumers Council - Two:" ]
            , p [] [ text (toString twos) ]
            , p [] [ text "Consumers Council - Three:" ]
            , p [] [ text (toString threes) ]
            ]


viewIterate : Model -> Html Msg
viewIterate model =
   div [] [ text "Fill me" ]

viewAddActorForm : Model -> Html Msg
viewAddActorForm model =
    div []
        [ p [] [ text "What is your name?" ]
        , input [ placeholder "Enter your name", onInput EnterName ] []
        , p [] [ text "What consumer council do you want to join?" ]
        , select [ onInput SelectCc ]
            [ option [ value "" ] [ text "Choose a consumer council:" ]
            , option [ value "1" ] [ text "1" ]
            , option [ value "2" ] [ text "2" ]
            , option [ value "3" ] [ text "3" ]
            ]
        , p [] [ text "What worker council do you want to join?" ]
        , select [ onInput SelectWc ]
            [ option [ value "" ] [ text "Choose a worker council:" ]
            , option [ value "pizza" ] [ text "pizza" ]
            , option [ value "beer" ] [ text "beer" ]
            ]
        , p [] []
        , button
            [ onClick
                (AddActor <|
                    newActor (List.length model.actors + 1)
                        model.tempName
                        model.tempCc
                        model.tempWc
                        10
                )
            ]
            [ text "Add Actor" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr []
                [ td []
                    [ button
                        [ onClick ShowAddActorPane ]
                        [ text "Add Actor" ]
                    , button
                        [ onClick ShowCouncilsPane ]
                        [ text "Show Councils" ]
                    , button
                        [ onClick ShowIterationPane ]
                        [ text "Iterate" ]
                    , button
                        [ onClick ShowDebugPane ]
                        [ text "Debug" ]
                    ]
                ]
            , td []
                [ h2 [] [ text "Participatory Economics Classroom Simulator" ]
                , case model.currentPane of
                    "AddActor" ->
                        viewAddActorForm model

                    "ShowCouncils" ->
                        viewCouncils model

                    "Iterate" ->
                        viewIterate model

                    _ ->
                        p [] [ text (toString model) ]
                ]
            ]
        ]
