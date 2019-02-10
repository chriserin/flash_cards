module Main exposing (ShowState(..), initFn, initialModel, main, subscriptionFn, updateFn, viewFn)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


type ShowState
    = Half
    | Full


type CardStartState
    = Back
    | Front


cards =
    [ { frontside = "The default port for the Mysql database server", backside = "port 5432" }
    , { frontside = "The http status code for successfully created", backside = "Http Status Code 201" }
    , { frontside = "The default port for the Rails development server", backside = "port 3000" }
    ]


initialModel =
    { card = { frontside = "The default port for the elm development server", backside = "port 1234" }, showState = Half, cardStart = Back, availableCards = cards }


type Msg
    = Click
    | NewCardIndex Int


initFn () =
    ( initialModel, Cmd.none )


updateFn msg model =
    case msg of
        Click ->
            case model.showState of
                Half ->
                    ( { model | showState = Full }, Cmd.none )

                Full ->
                    ( { model | showState = Half }, Random.generate NewCardIndex (Random.int 0 (List.length model.availableCards - 1)) )

        NewCardIndex cardIndex ->
            let
                slicedCardPair =
                    sliceCard model.availableCards cardIndex

                newCard =
                    case List.head (Tuple.first slicedCardPair) of
                        Nothing ->
                            model.card

                        Just cardIndexPair ->
                            Tuple.second cardIndexPair

                newAvailableCards =
                    Tuple.second (List.unzip (Tuple.second slicedCardPair))
            in
            ( { model | card = newCard, availableCards = newAvailableCards }, Cmd.none )


subscriptionFn model =
    Sub.none


sliceCard availableCards cardIndex =
    let
        indexedCards =
            List.indexedMap Tuple.pair availableCards

        partitionedList index =
            List.partition (\x -> Tuple.first x == index) indexedCards
    in
    partitionedList cardIndex


viewCardTextQuestion model =
    case model.cardStart of
        Front ->
            model.card.frontside

        Back ->
            model.card.backside


viewCardTextAnswer model =
    case model.cardStart of
        Front ->
            model.card.backside

        Back ->
            model.card.frontside


viewAnswerSide model =
    case model.showState of
        Full ->
            Html.div [ class "answer-side" ] [ text (viewCardTextAnswer model) ]

        Half ->
            Html.text ""


viewFn model =
    Html.div [ onClick Click ] [ Html.div [ class "question-side" ] [ text (viewCardTextQuestion model) ], viewAnswerSide model ]


main =
    Browser.element
        { init = initFn
        , update = updateFn
        , view = viewFn
        , subscriptions = subscriptionFn
        }
