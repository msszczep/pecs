module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, table, tr, td, h2, br, input, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)
import Debug exposing (toString)

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
    , consumptionThreshold : Float
    , hoursToWork : Float
    , maxLeisureTime : Float
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
    , tempConsumptionThreshold : Float
    }


newPriceVector : Prices
newPriceVector =
    Prices 10.0 10.0


init : Model
init =
    Model [] newPriceVector "" "" "" 0.0


newActor : Int -> String -> String -> Float -> Actor
newActor n cc wc ct =
    Actor n "" cc wc 0.0 0.0 ct 0.0 0.0



-- UPDATE


type Msg
    = AddActor Actor
    | ShowAddActorPane
    | ShowEditActorPane
    | SelectCc String
    | SelectWc String

update : Msg -> Model -> Model
update msg model =
    case msg of
        AddActor a ->
            { model | actors = a :: model.actors }

        SelectCc v ->
            { model | tempCc = v }

        SelectWc v ->
            { model | tempWc = v }

        ShowAddActorPane ->
            { model | currentPane = "AddActor" }

        ShowEditActorPane ->
            { model | currentPane = "EditActor" }

-- VIEW

viewAddActorForm : Model -> Html Msg
viewAddActorForm model =
  div [] 
  [ p [] [ text (toString model) ]
  , p [] [text "What consumer council do you want to join?"]
  , select [ onInput SelectCc ]
    [ option [value ""]  [text "Choose a consumer council:"]
    , option [value "1"] [text "1"]
    , option [value "2"] [text "2"]
    , option [value "3"] [text "3"]
    ]
  , p [] [ text "What worker council do you want to join?" ]
  , select [ onInput SelectWc ]
    [ option [value ""] [text "Choose a worker council:"]
    , option [value "pizza"] [ text "pizza"]
    , option [value "beer"] [ text "beer"]
    ]
  , p [] []
  , button
    [ onClick (AddActor <| newActor (List.length model.actors + 1)
                                  model.tempCc
                                  model.tempWc
                                  0.0) ]
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
                        [ text "Add Actor pane" ]
                    , button
                        [ onClick ShowEditActorPane ]
                        [ text "Edit Actor pane" ]
                    ]
                ]
            , td []
                [ h2 [] [ text "Participatory Economics Classroom Simulator" ]
                , if model.currentPane == "AddActor" then
                    viewAddActorForm model
                  else
                    p [] [ text (toString model) ]
                ]
            ]
        ]
