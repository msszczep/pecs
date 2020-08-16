module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, table, tr, td, h2, br, input, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder, style)
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
    , numBeersWanted : String
    , numPizzasWanted : String
    , hiLo : String
    , hoursToWork : String
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
    , tempHiLo : String
    , tempPizzas : String
    , tempBeers : String
    , tempWorkHours : String
    , tempQuestionFormId : Int
    }


newPriceVector : Prices
newPriceVector =
    Prices 10.0 10.0


init : Model
init =
    Model [] newPriceVector "" "" "" "" "" "" "" "" 0


newActor : Int -> String -> String -> String -> Int -> String -> Actor
newActor i name cc wc mlt hilo =
    Actor i name cc wc "0" "0" hilo "0" mlt



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
    | EnterHiLo String
    | SelectPizzas String
    | SelectBeers String
    | UpdateCCForm Int
    | UpdateWCForm Int
    | UpdateActor String
    | SelectWorkHours String


getOneActor : List Actor -> Actor
getOneActor actors =
    let
        result =
            List.head actors
    in
        case result of
            Just x ->
                x

            Nothing ->
                newActor 0 "" "" "" 10 ""


enterActorUpdate : String -> Model -> List Actor
enterActorUpdate councilType model =
    let
        remainingActors =
            List.filter (\e -> e.id /= model.tempQuestionFormId) model.actors

        actorToUpdate =
            List.filter (\e -> e.id == model.tempQuestionFormId) model.actors |> getOneActor

        replacedCc =
            { actorToUpdate
                | numPizzasWanted = model.tempPizzas
                , numBeersWanted = model.tempBeers
            }

        replacedWc =
            { actorToUpdate | hoursToWork = model.tempWorkHours }
    in
        if councilType == "wc" then
            replacedWc :: remainingActors
        else
            replacedCc :: remainingActors


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddActor a ->
            { model
                | actors = a :: model.actors
                , currentPane = "ShowCouncils"
            }

        SelectCc v ->
            { model | tempCc = v }

        SelectWc v ->
            { model | tempWc = v }

        SelectPizzas v ->
            { model | tempPizzas = v }

        SelectWorkHours v ->
            { model | tempWorkHours = v }

        SelectBeers v ->
            { model | tempBeers = v }

        EnterName s ->
            { model | tempName = s }

        EnterHiLo s ->
            { model | tempHiLo = s }

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

        UpdateCCForm i ->
            { model
                | tempQuestionFormId = i
                , currentPane = "ShowCCQuestionForm"
            }

        UpdateWCForm i ->
            { model
                | tempQuestionFormId = i
                , currentPane = "ShowWCQuestionForm"
            }

        UpdateActor councilType ->
            { model
                | actors = enterActorUpdate councilType model
                , currentPane = "ShowCouncils"
            }



-- VIEW
-- #cfc : light green
-- TODO: Change viewCouncil to adjust color based on completed status


viewCouncil : String -> ( Int, String ) -> Html Msg
viewCouncil councilType ( id, name ) =
    let
        onClickToUse =
            if councilType == "wc" then
                UpdateWCForm
            else
                UpdateCCForm
    in
        td
            [ style "border" "1px solid red"
            , style "padding" "10px"
            , style "background-color" "#f78181"
            ]
            [ p [] [ text name ]
            , button
                [ onClick (onClickToUse id) ]
                [ text "Submit Data" ]
            ]


viewCCQuestions : Model -> Html Msg
viewCCQuestions model =
    div []
        [ p [] [ text "How many pizzas do you want?" ]
        , select [ onInput SelectPizzas ]
            [ option [ value "0" ] [ text "Choose a number:" ]
            , option [ value "1" ] [ text "1" ]
            , option [ value "2" ] [ text "2" ]
            , option [ value "3" ] [ text "3" ]
            , option [ value "4" ] [ text "4" ]
            , option [ value "5" ] [ text "5" ]
            , option [ value "6" ] [ text "6" ]
            , option [ value "7" ] [ text "7" ]
            ]
        , p [] [ text "How many beers do you want?" ]
        , select [ onInput SelectBeers ]
            [ option [ value "0" ] [ text "Choose a number:" ]
            , option [ value "1" ] [ text "1" ]
            , option [ value "2" ] [ text "2" ]
            , option [ value "3" ] [ text "3" ]
            , option [ value "4" ] [ text "4" ]
            , option [ value "5" ] [ text "5" ]
            , option [ value "6" ] [ text "6" ]
            , option [ value "7" ] [ text "7" ]
            ]
        , p [] []
        , button
            [ onClick (UpdateActor "cc") ]
            [ text "Update Actor" ]
        ]


viewWCQuestions : Model -> Html Msg
viewWCQuestions model =
    div []
        [ p [] [ text "How many hours do you want to work?" ]
        , select [ onInput SelectWorkHours ]
            [ option [ value "0" ] [ text "Choose a number:" ]
            , option [ value "1" ] [ text "1" ]
            , option [ value "2" ] [ text "2" ]
            , option [ value "3" ] [ text "3" ]
            , option [ value "4" ] [ text "4" ]
            , option [ value "5" ] [ text "5" ]
            , option [ value "6" ] [ text "6" ]
            , option [ value "7" ] [ text "7" ]
            , option [ value "8" ] [ text "8" ]
            , option [ value "9" ] [ text "9" ]
            , option [ value "10" ] [ text "10" ]
            ]
        , p [] []
        , button
            [ onClick (UpdateActor "wc") ]
            [ text "Update Actor" ]
        ]



-- TODO : Add List.sortBy to end


viewCouncils : Model -> Html Msg
viewCouncils model =
    let
        pizzas =
            List.filter (\c -> c.wc == "pizza") model.actors |> List.map (\x -> ( x.id, x.name ))

        beers =
            List.filter (\c -> c.wc == "beer") model.actors |> List.map (\x -> ( x.id, x.name ))

        ones =
            List.filter (\c -> c.cc == "1") model.actors |> List.map (\x -> ( x.id, x.name ))

        twos =
            List.filter (\c -> c.cc == "2") model.actors |> List.map (\x -> ( x.id, x.name ))

        threes =
            List.filter (\c -> c.cc == "3") model.actors |> List.map (\x -> ( x.id, x.name ))
    in
        div []
            [ p [] [ text "Workers Council - Pizza:" ]
            , table []
                [ tr [] (List.map (viewCouncil "wc") pizzas)
                ]
            , p [] [ text "Workers Council - Beer:" ]
            , table []
                [ tr [] (List.map (viewCouncil "wc") beers)
                ]
            , p [] [ text "==========================================" ]
            , p [] [ text "Consumers Council - One:" ]
            , table []
                [ tr [] (List.map (viewCouncil "cc") ones)
                ]
            , p [] [ text "Consumers Council - Two:" ]
            , table []
                [ tr [] (List.map (viewCouncil "cc") twos)
                ]
            , p [] [ text "Consumers Council - Three:" ]
            , table []
                [ tr [] (List.map (viewCouncil "cc") threes)
                ]
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
        , p [] [ text "What is your hi-lo work threshold?" ]
        , input [ placeholder "Enter a number", onInput EnterHiLo ] []
        , p [] []
        , button
            [ onClick
                (AddActor <|
                    newActor (List.length model.actors + 1)
                        model.tempName
                        model.tempCc
                        model.tempWc
                        10
                        model.tempHiLo
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

                    "ShowCCQuestionForm" ->
                        viewCCQuestions model

                    "ShowWCQuestionForm" ->
                        viewWCQuestions model

                    _ ->
                        p [] [ text (toString model) ]
                ]
            ]
        ]
