module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Debug exposing (toString)


-- import Html.Attributes exposing (..)
-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL


type alias Actor =
    { id : Int
    , name : String
    , cc : Int
    , wc : Int
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
    }



newPriceVector : Prices
newPriceVector =
    Prices 10.0 10.0


init : Model
init =
    Model [] newPriceVector ""


newActor : Int -> Actor
newActor n =
    Actor n "" 0 0 0.0 0.0 0.0 0.0 0.0


-- UPDATE

type Msg
    = AddActor Actor
    | ShowAddActorPane
    | ShowEditActorPane


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddActor a ->
            { model | actors = a :: model.actors }
        ShowAddActorPane ->
            { model | currentPane = "AddActor" }
        ShowEditActorPane ->
            { model | currentPane = "EditActor" }


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick (AddActor <| newActor <| List.length model.actors + 1) ]
            [ text "Add Actor" ]
        , button 
            [ onClick ShowAddActorPane ]
            [ text "Add Actor pane"]
        , button 
            [ onClick ShowEditActorPane ]
            [ text "Edit Actor pane"]
        , p [] [ text (toString model) ]
        ]
