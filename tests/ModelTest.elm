module ModelTest exposing (..)

import Data.Currency as Currency
import Data.Resource as Resource
import Expect
import Model exposing (Model)
import Test exposing (..)


constantCostResource : Resource.Resource
constantCostResource =
    Resource.build "Constant cost resource" 1 1 (Currency.Currency 5)


doubleCostResource : Resource.Resource
doubleCostResource =
    Resource.build "Double cost resource" 1 2 (Currency.Currency 5)


initialModel : Model
initialModel =
    let
        initial =
            Model.initial
    in
    { initial | resources = [ constantCostResource, doubleCostResource ] }


initialModelWithAvailableFunds : Currency.Currency -> Model
initialModelWithAvailableFunds funds =
    let
        model =
            initialModel
    in
    { model | availableFunds = funds }


suite : Test
suite =
    describe "Model"
        [ describe "purchaseResource"
            [ test "disallows purchasing without enough funds" <|
                \_ ->
                    Expect.equal
                        (Model.purchaseResource 1 constantCostResource initialModel)
                        initialModel
            , test "allows purchasing with enough funds" <|
                \_ ->
                    Expect.equal
                        (Model.purchaseResource 1 constantCostResource (initialModelWithAvailableFunds <| Currency.Currency 5)).availableFunds
                        Currency.zero
            , test "allows purchasing multiple with enough funds" <|
                \_ ->
                    Expect.equal
                        (Model.purchaseResource 4 constantCostResource (initialModelWithAvailableFunds <| Currency.Currency 250)).availableFunds
                        (Currency.Currency 230)
            , test "maintains updated costs if the resource has a multiplier" <|
                \_ ->
                    Expect.equal
                        (Model.purchaseResource 4 doubleCostResource (initialModelWithAvailableFunds <| Currency.Currency 250)).availableFunds
                        (Currency.Currency 175)
            ]
        ]
