module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, p, table, tr, td, th, h2, br, b, input, select, option)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, placeholder, style)
import Debug exposing (toString)
import Tuple exposing (first, second)
import Markdown
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


type alias Model =
    { actors : List Actor
    , beerPrice : Float
    , pizzaPrice : Float
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
    , iteration : Int
    }


init : Model
init =
    Model [] 10.0 10.0 "" "" "" "" "" "" "" "" 0 [] 0


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
      , numBeersWanted = "4"
      , numPizzasWanted = "5"
      , wc = "beer"
      }
    , { cc = "2"
      , hiLo = "2"
      , hoursToWork = "2"
      , id = 2
      , maxLeisureTime = 10
      , name = "Zachary"
      , numBeersWanted = "3"
      , numPizzasWanted = "4"
      , wc = "pizza"
      }
    , { cc = "2"
      , hiLo = "8"
      , hoursToWork = "8"
      , id = 4
      , maxLeisureTime = 10
      , name = "Chris"
      , numBeersWanted = "4"
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
    | ResetIteration
    | ResetAiIteration


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

        ResetIteration ->
            { model
                | iterationsArchive = model.actors :: model.iterationsArchive
                , actors = List.map resetActor model.actors
                , beerPrice = adjustPrice "beer" model
                , pizzaPrice = adjustPrice "pizza" model
            }

        ResetAiIteration ->
            { model
                | iterationsArchive = model.actors :: model.iterationsArchive
                , actors = updateAiActors model
                , beerPrice = adjustPrice "beer" model
                , pizzaPrice = adjustPrice "pizza" model
                , iteration = model.iteration + 1
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


numActors : String -> String -> List Actor -> String
numActors councilType councilId actors =
    case councilType of
        "wc" ->
            List.filter (\x -> x.wc == councilId) actors |> List.length |> toString

        _ ->
            List.filter (\x -> x.cc == councilId) actors |> List.length |> toString


viewCCQuestions : Model -> Html Msg
viewCCQuestions model =
    let
        currentActor =
            List.filter (\e -> e.id == model.tempQuestionFormId) model.actors |> getOneActor
    in
        div []
            [ p [] [ text (currentActor.name ++ ", how many pizzas do you want?") ]
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
    let
        currentActor =
            List.filter (\e -> e.id == model.tempQuestionFormId) model.actors |> getOneActor
    in
        div []
            [ p [] [ text (currentActor.name ++ ", how many hours do you want to work?") ]
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


quickIntConvert : String -> Int
quickIntConvert s =
    String.toInt s |> Maybe.withDefault 0


computeCcBudget : String -> String -> List Actor -> String
computeCcBudget id hiLoThreshold actors =
    let
        hiLoTemp =
            quickIntConvert hiLoThreshold

        actorsInCouncil =
            actors
                |> List.filter (\c -> c.cc == id)
                |> List.filter (\c -> c.hoursToWork /= "0")

        hiLoCouncils =
            List.partition (\x -> (quickIntConvert x.hoursToWork) < hiLoTemp) actorsInCouncil
    in
        (List.length (first hiLoCouncils) * 50) + (List.length (second hiLoCouncils) * 100) |> String.fromInt


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


resetActor : Actor -> Actor
resetActor a =
    { a
        | hoursToWork = "0"
        , numBeersWanted = "0"
        , numPizzasWanted = "0"
    }


updateAiActor : Bool -> Bool -> Actor -> Actor
updateAiActor didBeerPriceDecrease didPizzaPriceDecrease actor =
    let
        adjustNumBeersWanted =
            if didBeerPriceDecrease then
                1
            else
                (-1)

        adjustNumPizzasWanted =
            if didPizzaPriceDecrease then
                1
            else
                (-1)

        adjustHoursToWork =
            case actor.wc of
                "pizza" ->
                    if didPizzaPriceDecrease then
                        (-1)
                    else
                        1

                _ ->
                    if didBeerPriceDecrease then
                        (-1)
                    else
                        1
    in
        { actor
            | numBeersWanted = (quickIntConvert actor.numBeersWanted) + adjustNumBeersWanted |> String.fromInt
            , numPizzasWanted = (quickIntConvert actor.numPizzasWanted) + adjustNumPizzasWanted |> String.fromInt
            , hoursToWork = (quickIntConvert actor.hoursToWork) + adjustHoursToWork |> String.fromInt
        }


updateAiActors : Model -> List Actor
updateAiActors model =
    let
        oldBeerPrice =
            model.beerPrice

        oldPizzaPrice =
            model.pizzaPrice

        newPizzaPrice =
            adjustPrice "pizza" model

        newBeerPrice =
            adjustPrice "beer" model

        didBeerPriceDecrease =
            oldBeerPrice > newBeerPrice

        didPizzaPriceDecrease =
            oldPizzaPrice > newPizzaPrice
    in
        List.map (updateAiActor didBeerPriceDecrease didPizzaPriceDecrease) model.actors


sumWorkHours : String -> List Actor -> String
sumWorkHours t actors =
    actors
        |> List.filter (\c -> c.wc == t)
        |> List.map .hoursToWork
        |> List.map String.toInt
        |> List.map (Maybe.withDefault 0)
        |> List.foldl (+) 0
        |> String.fromInt


calculatePizzaSupply : String -> String
calculatePizzaSupply pizzaHours =
    pizzaHours
        |> String.toInt
        |> Maybe.withDefault 0
        |> (*) 2
        |> String.fromInt


calculateDemand : String -> List Actor -> String
calculateDemand t actors =
    let
        goodToUse =
            if t == "beer" then
                .numBeersWanted
            else
                .numPizzasWanted
    in
        actors
            |> List.map goodToUse
            |> List.map String.toInt
            |> List.map (Maybe.withDefault 0)
            |> List.foldl (+) 0
            |> String.fromInt


adjustPrice : String -> Model -> Float
adjustPrice t model =
    let
        pizzaHours =
            sumWorkHours "pizza" model.actors

        beerHours =
            sumWorkHours "beer" model.actors

        supply =
            (if t == "pizza" then
                calculatePizzaSupply pizzaHours
             else
                beerHours
            )
                |> String.toInt
                |> Maybe.withDefault 0

        demand =
            calculateDemand t model.actors |> String.toInt |> Maybe.withDefault 0

        priceToAdjust =
            if t == "pizza" then
                model.pizzaPrice
            else
                model.beerPrice

        adjustmentValue =
            if demand > supply then
                2
            else
                (-2)
    in
        priceToAdjust + adjustmentValue


showStatsTables : Model -> List (Html Msg)
showStatsTables model =
    let
        pizzaHours =
            sumWorkHours "pizza" model.actors

        beerHours =
            sumWorkHours "beer" model.actors

        pizzaSupply =
            calculatePizzaSupply pizzaHours

        beerSupply =
            beerHours

        pizzaDemand =
            calculateDemand "pizza" model.actors

        beerDemand =
            calculateDemand "beer" model.actors

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
                - (model.beerPrice * (quickFloatConvert (demandsByCouncilAndGood "1" "beer" model.actors)))
                - (model.pizzaPrice * (quickFloatConvert (demandsByCouncilAndGood "1" "pizza" model.actors)))

        ( cc1BudgetSurplusShow, cc1BudgetSurplusStyle ) =
            if cc1BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        cc2BudgetSurplus =
            (quickFloatConvert cc2budget)
                - (model.beerPrice * (quickFloatConvert (demandsByCouncilAndGood "2" "beer" model.actors)))
                - (model.pizzaPrice * (quickFloatConvert (demandsByCouncilAndGood "2" "pizza" model.actors)))

        ( cc2BudgetSurplusShow, cc2BudgetSurplusStyle ) =
            if cc2BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        cc3BudgetSurplus =
            (quickFloatConvert cc3budget)
                - (model.beerPrice * (quickFloatConvert (demandsByCouncilAndGood "3" "beer" model.actors)))
                - (model.pizzaPrice * (quickFloatConvert (demandsByCouncilAndGood "3" "pizza" model.actors)))

        ( cc3BudgetSurplusShow, cc3BudgetSurplusStyle ) =
            if cc3BudgetSurplus >= 0 then
                ( "OK", "green" )
            else
                ( "FAIL", "red" )

        isIterationComplete =
            List.all isActorComplete model.actors && (List.length model.actors > 0)

        isIterationCompleteShow =
            if isIterationComplete then
                "complete"
            else
                "incomplete"

        isIterationSuccessful =
            beerRangeResult && pizzaRangeResult && (cc1BudgetSurplus >= 0) && (cc2BudgetSurplus >= 0) && (cc3BudgetSurplus >= 0)

        isIterationSuccessfulShow =
            if isIterationSuccessful then
                "YES"
            else
                "NO"

        iterationButton =
            if isIterationComplete == True && isIterationSuccessful == False then
                button [ onClick ResetIteration ] [ text "Iterate" ]
            else
                div [] []

        iterationAiButton =
            if isIterationComplete == True && isIterationSuccessful == False then
                button [ onClick ResetAiIteration ] [ text "AI--Iterate" ]
            else
                div [] []

        tdStyle =
            [ style "border" "1px solid #ddd"
            , style "padding" "8px"
            ]

        thStyle =
            [ style "padding" "8px"
            , style "text-align" "center"
            , style "background-color" "#23238E"
            , style "color" "white"
            ]
    in
        [ td [ style "vertical-align" "text-top" ]
            [ div []
                [ table []
                    [ tr []
                        [ th thStyle [ text "Goods" ]
                        , th thStyle [ text "Price" ]
                        , th thStyle [ text "Supply" ]
                        , th thStyle [ text "Demand" ]
                        , th thStyle [ text "Range" ]
                        , th thStyle [ text "OK?" ]
                        ]
                    , tr []
                        [ td tdStyle [ text "Pizza" ]
                        , td tdStyle [ text (toString model.pizzaPrice) ]
                        , td tdStyle [ text pizzaSupply ]
                        , td tdStyle [ text pizzaDemand ]
                        , td tdStyle [ text (pizzaRangeMin ++ " - " ++ pizzaRangeMax) ]
                        , td tdStyle
                            [ b [ style "color" pizzaRangeResultStyle ]
                                [ text pizzaRangeResultShow ]
                            ]
                        ]
                    , tr []
                        [ td tdStyle [ text "Beer" ]
                        , td tdStyle [ text (toString model.beerPrice) ]
                        , td tdStyle [ text beerSupply ]
                        , td tdStyle [ text beerDemand ]
                        , td tdStyle [ text (beerRangeMin ++ " - " ++ beerRangeMax) ]
                        , td tdStyle
                            [ b [ style "color" beerRangeResultStyle ]
                                [ text beerRangeResultShow ]
                            ]
                        ]
                    ]
                , p [] []
                , table []
                    [ tr []
                        [ th thStyle [ text "CC" ]
                        , th thStyle [ text "#Actors" ]
                        , th thStyle [ text "Budget" ]
                        , th thStyle [ text "Surplus" ]
                        , th thStyle [ text "OK?" ]
                        ]
                    , tr []
                        [ td tdStyle [ text "CC1" ]
                        , td tdStyle [ text (numActors "cc" "1" model.actors) ]
                        , td tdStyle [ text cc1budget ]
                        , td tdStyle [ text (toString cc1BudgetSurplus) ]
                        , td tdStyle
                            [ b [ style "color" cc1BudgetSurplusStyle ]
                                [ text cc1BudgetSurplusShow ]
                            ]
                        ]
                    , tr []
                        [ td tdStyle [ text "CC2" ]
                        , td tdStyle [ text (numActors "cc" "2" model.actors) ]
                        , td tdStyle [ text cc2budget ]
                        , td tdStyle [ text (toString cc2BudgetSurplus) ]
                        , td tdStyle
                            [ b [ style "color" cc2BudgetSurplusStyle ]
                                [ text cc2BudgetSurplusShow ]
                            ]
                        ]
                    , tr []
                        [ td tdStyle [ text "CC3" ]
                        , td tdStyle [ text (numActors "cc" "3" model.actors) ]
                        , td tdStyle [ text cc3budget ]
                        , td tdStyle [ text (toString cc3BudgetSurplus) ]
                        , td tdStyle
                            [ b [ style "color" cc3BudgetSurplusStyle ]
                                [ text cc3BudgetSurplusShow ]
                            ]
                        ]
                    ]
                , p [] []
                , table []
                    [ tr []
                        [ th thStyle [ text "WC" ]
                        , th thStyle [ text "#Actors" ]
                        , th thStyle [ text "Hours" ]
                        ]
                    , tr []
                        [ td tdStyle [ text "Pizza" ]
                        , td tdStyle [ text (numActors "wc" "pizza" model.actors) ]
                        , td tdStyle [ text pizzaHours ]
                        ]
                    , tr []
                        [ td tdStyle [ text "Beer" ]
                        , td tdStyle [ text (numActors "wc" "beer" model.actors) ]
                        , td tdStyle [ text beerHours ]
                        ]
                    ]
                , p [] []
                , table []
                    [ tr []
                        [ th thStyle [ text "HiLo Threshold" ]
                        , th thStyle [ text "Iteration" ]
                        ]
                    , tr []
                        [ td tdStyle [ text hiLoAvg ]
                        , td tdStyle [ text (toString model.iteration) ]
                        ]
                    ]
                , p [] []
                , iterationButton
                , p [] []
                , iterationAiButton
                ]
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
                (List.append (showStatsTables model)
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


instructions : String
instructions =
    """
This is a computerized version of the Participatory Economics Classroom Simulator (PECS),
programmed by Mitchell Szczepanczyk, devised by Justin Jarvis, and based on the economic
model of a participatory economy co-invented by Michael Albert and Robin Hahnel.

A participatory economy is an economic model of democratic planning, where participants craft
a society-wide production, consumption, and allocation plan -- without the retentionist
economic institutions of markets or command planning.  This is done by means of worker
and consumer councils -- decision-making bodies for groups of individuals to decide what
and how much to produce and what and how much to consume.  These councils interact via a participatory
planning procedure to determine an allocation plan in one or more rounds ("iterations").

In PECS, each individual joins a worker council to produce one of two goods (either pizza or beer)
and a consumer council to aggregate the consumption of those goods.

You can switch from one page to the next by clicking any of the buttons at the top of this
page.  CAUTION: **_If you refresh the page or close the browser, doing so will delete the app's
current state, so be careful_**.

Each individual participant ("actor") in PECS joins the simluation by clicking the "Add Actor" button
at the top of the page.  Each actor answers some questions one time: their name, which consumer
council they wish to join (identified simply by number -- 1, 2 or 3), which worker council they wish to join
(identified by the good they produce -- either pizza or beer), and a "HiLo" threshold
(a number to help calculate payment based on the number of hours each actor works).  Each actor
then clicks the "Add Actor" button to join PECS.

Upon joining PECS, each actor then sees a scoreboard of the state of the allocation plan in the current
iteration, as well as a number of related statistics.  You can also see this scoreboard by clicking the "Show
Stats / Councils" button at the top of the page.  Each actor appears twice on the right-hand side of the
scoreboard: in the list of worker councils, and in the list of consumer councils.  By default, the
name appears inside of a red box with a corresponding "Enter Data" button.  When you click the "Enter Data" button,
you're asked one or more questions, depending on the council type.  For a worker council, the actor is asked how
many hours you want to work.  For a consumer council, the actor is asked how many pizzas and how many beers
they wish to consume.  Each actor then clicks the "Update Actor" button to submit their response.

After an actor submits their response, the "Enter Data" button disppears and the red box is now highlighted green.
That shows that the actor has submitted their portion of the allocation plan for that council.
The scoreboard provides a summary of the state of the allocation plan for a given iteration -- aggregating the
number of hours consumed, the budgets of each consumer council, the HiLo threshold (the average of all HiLo scores
from all actors), the supply/demand/price of each good, and the overall iteration status.

The production of each good takes only the number hours of labor as input.  A single pizza takes a half-hour of
labor as input; a single beer takes one hour of labor as input.  Each actor gets either 50 points if their number of work
hours is below the HiLo threshold, or 100 points if their number of work hours is at or above the HiLo threshold.
The budget of each consumer council is the sum of the number of points of that council's actors.  The cost of
all the goods for a consumer council is the price of each good multiplied by the quantity of that good.  The
starting price of each good is ten points.

The supply and demand of each good must be within ten percent of each other.  The acceptable quantity range
for each good appears in the "Final iteration status" column on the scoreboard.  The simulation ends when the
supply and demand of each good is within ten percent of each other _and_ when each consumer council is at or under
its budget.  We then have a production plan and production can begin.  If we don't have a production plan,
an "Iterate" button appears on the page; if you click on that, you archive the current iteration round and start
a new round where you ask each actor to revise the number of work hours and the number of goods to consume.
In addition, a good's price is adjusted down two points if the supply for that good is greater than its demand,
or increased two points if demand is greater than supply.

The "Debug" button at the top shows the current state of the app at any given point.  The source code for the app
is written in the Elm programming language and is [online here](https://github.com/msszczep/pecs).

   """


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
                        [ text "Show Stats / Councils" ]
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
                        Markdown.toHtml
                            [ style "padding" "20px"
                            ]
                            instructions
                ]
            ]
        ]
