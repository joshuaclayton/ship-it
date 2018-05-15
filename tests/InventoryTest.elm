module InventoryTest exposing (..)

import Data.Currency as Currency
import Data.Inventory as Inventory exposing (Inventory)
import Data.Resource as Resource
import Expect
import Test exposing (..)


constantCostResource : Resource.Resource
constantCostResource =
    Resource.build "Constant cost resource" 1 1 (Currency.Currency 5)


doubleCostResource : Resource.Resource
doubleCostResource =
    Resource.build "Double cost resource" 1 2 (Currency.Currency 5)


initialModel : Inventory
initialModel =
    Inventory.initial
        |> Inventory.replaceResources [ constantCostResource, doubleCostResource ]


initialModelWithAvailableFunds : Currency.Currency -> Inventory
initialModelWithAvailableFunds funds =
    initialModel
        |> Inventory.setAvailableFunds funds


suite : Test
suite =
    describe "Model"
        [ describe "purchaseResource"
            [ test "disallows purchasing without enough funds" <|
                \_ ->
                    Expect.equal
                        (Inventory.purchaseResource 1 constantCostResource initialModel)
                        initialModel
            , test "allows purchasing with enough funds" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 1 constantCostResource (initialModelWithAvailableFunds <| Currency.Currency 5))
                        Currency.zero
            , test "allows purchasing multiple with enough funds" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 4 constantCostResource (initialModelWithAvailableFunds <| Currency.Currency 250))
                        (Currency.Currency 230)
            , test "maintains updated costs if the resource has a multiplier" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 4 doubleCostResource (initialModelWithAvailableFunds <| Currency.Currency 250))
                        (Currency.Currency 175)
            ]
        ]
