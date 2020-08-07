import Browser
import Html exposing (Html, button, div, text, p)
-- import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug exposing (toString)

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
  } 

newPriceVector : Prices
newPriceVector = Prices 10.0 10.0

init : Model
init =
  Model [] newPriceVector

newActor : Actor
newActor =
  Actor 0 "" 0 0 0.0 0.0 0.0 0.0 0.0

-- UPDATE

type Msg
  = AddActor Actor

update : Msg -> Model -> Model
update msg model =
  case msg of
    AddActor a ->
      { model | actors = a :: model.actors }

-- VIEW

view : Model -> Html Msg
view model =
  div [] 
      [ 
        button
        [ onClick (AddActor newActor) ]
        [ text "Add Actor" ]
      , p [] [ text (toString model) ]
      ]


