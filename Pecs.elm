module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, table, tr, td, h2, br, b, input, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder, style)
import Debug exposing (toString)
import Tuple exposing (first, second)
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
    , iterationsArchive : List (List Actor)
    }


newPriceVector : Prices
newPriceVector =
    Prices 10.0 10.0


init : Model
init =
    Model [] newPriceVector "" "" "" "" "" "" "" "" 0 []


newActor : Int -> String -> String -> String -> Int -> String -> Actor
newActor i name cc wc mlt hilo =
    Actor i name cc wc "0" "0" hilo "0" mlt


testActors : List Actor
testActors =
    [ { cc = "3"
      , hiLo = "7"
      , hoursToWork = "8"
      , id = 3
      , maxLeisureTime = 10
      , name = "Jason"
      , numBeersWanted = "3"
      , numPizzasWanted = "5"
      , wc = "beer"
      }
    , { cc = "2"
      , hiLo = "2"
      , hoursToWork = "2"
      , id = 2
      , maxLeisureTime = 10
      , name = "Zachary"
      , numBeersWanted = "1"
      , numPizzasWanted = "4"
      , wc = "pizza"
      }
    , { cc = "2"
      , hiLo = "8"
      , hoursToWork = "8"
      , id = 4
      , maxLeisureTime = 10
      , name = "Chris"
      , numBeersWanted = "3"
      , numPizzasWanted = "4"
      , wc = "beer"
      }
    , { cc = "1"
      , hiLo = "4"
      , hoursToWork = "8"
      , id = 1
      , maxLeisureTime = 10
      , name = "Mitchell"
      , numBeersWanted = "1"
      , numPizzasWanted = "6"
      , wc = "pizza"
      }
    ]



-- UPDATE


type Msg
    = AddActor Actor
    | ShowAddActorPane
    | ShowEditActorPane
    | ShowCouncilsPane
    | ShowDebugPane
    | ShowInstructionsPane
    | ApplyTestActors
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

        ShowInstructionsPane ->
            { model | currentPane = "" }

        ShowAddActorPane ->
            { model | currentPane = "AddActor" }

        ShowEditActorPane ->
            { model | currentPane = "EditActor" }

        ShowCouncilsPane ->
            { model | currentPane = "ShowCouncils" }

        ShowDebugPane ->
            { model | currentPane = "ShowDebugPanel" }

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

        ApplyTestActors ->
            { model
                | actors = testActors
                , currentPane = "ShowCouncils"
            }



-- VIEW


viewCouncil : String -> Actor -> Html Msg
viewCouncil councilType actor =
    let
        onClickToUse =
            if councilType == "wc" then
                UpdateWCForm
            else
                UpdateCCForm

        color =
            if
                (councilType == "wc" && actor.hoursToWork /= "0")
                    || (councilType == "cc" && actor.numBeersWanted /= "0" && actor.numPizzasWanted /= "0")
            then
                "1px solid green"
            else
                "1px solid red"

        colorCode =
            if
                (councilType == "wc" && actor.hoursToWork /= "0")
                    || (councilType == "cc" && actor.numBeersWanted /= "0" && actor.numPizzasWanted /= "0")
            then
                "#cfc"
            else
                "#f78181"

        buttonToUse =
            if
                (councilType == "wc" && actor.hoursToWork == "0")
                    || (councilType == "cc" && actor.numBeersWanted == "0" && actor.numPizzasWanted == "0")
            then
                button
                    [ onClick (onClickToUse actor.id) ]
                    [ text "Enter Data" ]
            else
                div [] []

        wcVerbiage =
            if actor.hoursToWork /= "0" then
                "(" ++ actor.hoursToWork ++ " hrs.)"
            else
                ""

        ccVerbiage =
            if actor.numBeersWanted /= "0" && actor.numPizzasWanted /= "0" then
                "(" ++ actor.numPizzasWanted ++ " pizzas, " ++ actor.numBeersWanted ++ " beers)"
            else
                ""

        verbiage =
            case councilType of
                "wc" ->
                    wcVerbiage

                _ ->
                    ccVerbiage
    in
        td
            [ style "border" color
            , style "padding" "5px"
            , style "background-color" colorCode
            ]
            [ p [] [ text <| actor.name ++ " " ++ verbiage ]
            , buttonToUse
            ]



-- TODO Add name in output:


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



-- # TODO: Add name in output:


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



-- TODO : Fix hoursToWork and hiLoThreshold to be numbers


computeCcBudget : String -> String -> List Actor -> String
computeCcBudget id hiLoThreshold actors =
    let
        actorsInCouncil =
            actors
                |> List.filter (\c -> c.cc == id)
                |> List.filter (\c -> c.hoursToWork /= "0")

        hiLoCouncils =
            List.partition (\x -> x.hoursToWork < hiLoThreshold) actorsInCouncil
    in
        (List.length (first hiLoCouncils) * 50) + (List.length (second hiLoCouncils) * 100) |> String.fromInt


quickIntConvert : String -> Int
quickIntConvert s =
    String.toInt s |> Maybe.withDefault 0


quickFloatConvert : String -> Float
quickFloatConvert s =
    String.toInt s |> Maybe.withDefault 0 |> toFloat


demandsByCouncilAndGood : String -> String -> List Actor -> String
demandsByCouncilAndGood id good actors =
    let
        goodToUse =
            if good == "beer" then
                .numBeersWanted
            else
                .numPizzasWanted
    in
        actors
            |> List.filter (\c -> c.cc == id)
            |> List.map goodToUse
            |> List.map String.toInt
            |> List.map (Maybe.withDefault 0)
            |> List.foldl (+) 0
            |> String.fromInt


priceRangeFunction : String -> String -> ( Bool, ( String, String ) )
priceRangeFunction val1 val2 =
    let
        val1Int =
            quickIntConvert val1

        val2Int =
            quickIntConvert val2

        maxQty =
            max val1Int val2Int |> toFloat

        minQty =
            min val1Int val2Int |> toFloat

        maxFinalRange =
            maxQty * 1.1

        minFinalRange =
            maxQty * 0.9

        isInRange =
            minQty < maxFinalRange && minQty > minFinalRange
    in
        ( isInRange, ( String.fromFloat minFinalRange, String.fromFloat maxFinalRange ) )


isActorComplete : Actor -> Bool
isActorComplete a =
    a.hoursToWork /= "0" && a.numBeersWanted /= "0" && a.numPizzasWanted /= "0"


showStats : Model -> List (Html msg)
showStats model =
    let
        pizzaHours =
            model.actors
                |> List.filter (\c -> c.wc == "pizza")
                |> List.map .hoursToWork
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
                |> List.foldl (+) 0
                |> String.fromInt

        beerHours =
            model.actors
                |> List.filter (\c -> c.wc == "beer")
                |> List.map .hoursToWork
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
                |> List.foldl (+) 0
                |> String.fromInt

        pizzaSupply =
            pizzaHours
                |> String.toInt
                |> Maybe.withDefault 0
                |> (*) 2
                |> String.fromInt

        beerSupply =
            beerHours

        pizzaDemand =
            model.actors
                |> List.map .numPizzasWanted
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
                |> List.foldl (+) 0
                |> String.fromInt

        beerDemand =
            model.actors
                |> List.map .numBeersWanted
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
                |> List.foldl (+) 0
                |> String.fromInt

        hiLoSum =
            model.actors
                |> List.map .hiLo
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
                |> List.foldl (+) 0

        hiLoAvg =
            (toFloat hiLoSum) / (toFloat (List.length model.actors)) |> String.fromFloat

        cc1budget =
            computeCcBudget "1" hiLoAvg model.actors

        cc2budget =
            computeCcBudget "2" hiLoAvg model.actors

        cc3budget =
            computeCcBudget "3" hiLoAvg model.actors

        ( beerRangeResult, ( beerRangeMin, beerRangeMax ) ) =
            priceRangeFunction beerDemand beerSupply

        ( pizzaRangeResult, ( pizzaRangeMin, pizzaRangeMax ) ) =
            priceRangeFunction pizzaDemand pizzaSupply

        ( beerRangeResultShow, beerRangeResultStyle ) =
            if beerRangeResult == True then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        ( pizzaRangeResultShow, pizzaRangeResultStyle ) =
            if pizzaRangeResult == True then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        cc1BudgetSurplus =
            (quickFloatConvert cc1budget)
                - (model.prices.beer * (quickFloatConvert (demandsByCouncilAndGood "1" "beer" model.actors)))
                - (model.prices.pizza * (quickFloatConvert (demandsByCouncilAndGood "1" "pizza" model.actors)))

        ( cc1BudgetSurplusShow, cc1BudgetSurplusStyle ) =
            if cc1BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        cc2BudgetSurplus =
            (quickFloatConvert cc2budget)
                - (model.prices.beer * (quickFloatConvert (demandsByCouncilAndGood "2" "beer" model.actors)))
                - (model.prices.pizza * (quickFloatConvert (demandsByCouncilAndGood "2" "pizza" model.actors)))

        ( cc2BudgetSurplusShow, cc2BudgetSurplusStyle ) =
            if cc2BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        cc3BudgetSurplus =
            (quickFloatConvert cc3budget)
                - (model.prices.beer * (quickFloatConvert (demandsByCouncilAndGood "3" "beer" model.actors)))
                - (model.prices.pizza * (quickFloatConvert (demandsByCouncilAndGood "3" "pizza" model.actors)))

        ( cc3BudgetSurplusShow, cc3BudgetSurplusStyle ) =
            if cc3BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        isIterationComplete = List.all isActorComplete model.actors && (List.length model.actors > 0)

        isIterationCompleteShow = if isIterationComplete then "complete" else "incomplete"

        isIterationSuccessful = beerRangeResult && pizzaRangeResult && (cc1BudgetSurplus >= 0) && (cc2BudgetSurplus >= 0) && (cc3BudgetSurplus >= 0)

        isIterationSuccessfulShow = if isIterationSuccessful then "YES" else "NO"

    in
        [ td
            [ style "padding" "30px"
            , style "vertical-align" "text-top"
            ]
            [ b [] [ text "Work council stats" ]
            , p [] [ text ("Pizza council workhours: " ++ pizzaHours) ]
            , p [] [ text ("Beer council workhours: " ++ beerHours) ]
            , b [] [ text "Consumer council budgets:" ]
            , p [] [ text ("CC1: " ++ cc1budget) ]
            , p [] [ text ("CC2: " ++ cc2budget) ]
            , p [] [ text ("CC3: " ++ cc3budget) ]
            , b [] [ text ("HiLo Threshold: " ++ hiLoAvg) ]
            ]
        , td
            [ style "padding" "30px"
            , style "vertical-align" "text-top"
            ]
            [ b [] [ text "Prices" ]
            , p [] [ text ("Pizza: " ++ String.fromFloat model.prices.pizza) ]
            , p [] [ text ("Beer: " ++ String.fromFloat model.prices.beer) ]
            , b [] [ text "Supply stats" ]
            , p [] [ text ("Pizza supply: " ++ pizzaSupply) ]
            , p [] [ text ("Beer supply: " ++ beerSupply) ]
            , b [] [ text "Demand stats" ]
            , p [] [ text ("Pizza demand: " ++ pizzaDemand) ]
            , p [] [ text ("Beer demand: " ++ beerDemand) ]
            ]
        , td
            [ style "padding" "30px"
            , style "vertical-align" "text-top"
            ]
            [ b [] [ text "Final iteration status" ]
            , p [] [ text ("Iteration " ++ isIterationCompleteShow) ]
            , p [] [ text ("Pizza Results: " ++ pizzaRangeMin ++ " | " ++ pizzaRangeMax) ]
            , b [ style "color" pizzaRangeResultStyle ]
                [ text pizzaRangeResultShow ]
            , p [] [ text ("Beer Results: " ++ beerRangeMin ++ " | " ++ beerRangeMax) ]
            , b [ style "color" beerRangeResultStyle ]
                [ text beerRangeResultShow ]
            , p [] [ text ("CC1 budget surplus: " ++ String.fromFloat cc1BudgetSurplus) ]
            , b [ style "color" cc1BudgetSurplusStyle ]
                [ text cc1BudgetSurplusShow ]
            , p [] [ text ("CC2 budget surplus: " ++ String.fromFloat cc2BudgetSurplus) ]
            , b [ style "color" cc2BudgetSurplusStyle ]
                [ text cc2BudgetSurplusShow ]
            , p [] [ text ("CC3 budget surplus: " ++ String.fromFloat cc3BudgetSurplus) ]
            , b [ style "color" cc3BudgetSurplusStyle ]
                [ text cc3BudgetSurplusShow ]
            , p [] []
            , b [] [text ("Successful? " ++ isIterationSuccessfulShow)]
            ]
        ]


viewCouncils : Model -> Html Msg
viewCouncils model =
    let
        pizzas =
            List.filter (\c -> c.wc == "pizza") model.actors |> List.sortBy .name

        beers =
            List.filter (\c -> c.wc == "beer") model.actors |> List.sortBy .name

        ones =
            List.filter (\c -> c.cc == "1") model.actors |> List.sortBy .name

        twos =
            List.filter (\c -> c.cc == "2") model.actors |> List.sortBy .name

        threes =
            List.filter (\c -> c.cc == "3") model.actors |> List.sortBy .name
    in
        table []
            [ tr []
                (List.append (showStats model)
                    [ td
                        [ style "padding" "30px"
                        , style "vertical-align" "text-top"
                        ]
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
                    ]
                )
            ]


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
                        [ onClick ShowInstructionsPane ]
                        [ text "Instructions" ]
                    , button
                        [ onClick ShowAddActorPane ]
                        [ text "Add Actor" ]
                    , button
                        [ onClick ShowCouncilsPane ]
                        [ text "Show Councils" ]
                    , button
                        [ onClick ShowDebugPane ]
                        [ text "Debug" ]
                    , button
                        [ onClick ApplyTestActors ]
                        [ text "Test Actors" ]
                    ]
                ]
            , td []
                [ h2 [] [ text "Participatory Economics Classroom Simulator" ]
                , case model.currentPane of
                    "AddActor" ->
                        viewAddActorForm model

                    "ShowCouncils" ->
                        viewCouncils model

                    "ShowCCQuestionForm" ->
                        viewCCQuestions model

                    "ShowWCQuestionForm" ->
                        viewWCQuestions model

                    "ShowDebugPanel" ->
                        p [] [ text (toString model) ]

                    _ ->
                        p [] [ text "instructions go here" ]
                ]
            ]
        ]
