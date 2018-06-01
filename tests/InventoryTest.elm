module InventoryTest exposing (..)

import Data.Currency as Currency
import Data.GameConfiguration as Config
import Data.IncomeRate as IncomeRate
import Data.Increasable as Increasable
import Data.Inventory as Inventory exposing (Inventory)
import Data.Resource as Resource
import Expect
import Test exposing (..)


constantCostResource : Resource.Resource
constantCostResource =
    Resource.build
        "Constant cost resource"
        (IncomeRate.build 1)
        (Increasable.buildMultiplier 1)
        (Currency.Currency 5)


doubleCostResource : Resource.Resource
doubleCostResource =
    Resource.build
        "Double cost resource"
        (IncomeRate.build 1)
        (Increasable.buildMultiplier 2)
        (Currency.Currency 5)


initialModel : Inventory
initialModel =
    Inventory.initialWithResources
        [ ( Config.L1, constantCostResource )
        , ( Config.L2, doubleCostResource )
        ]


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
                        (Inventory.purchaseResource 1 Config.L1 initialModel)
                        initialModel
            , test "allows purchasing with enough funds" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 1 Config.L1 (initialModelWithAvailableFunds <| Currency.Currency 5))
                        Currency.zero
            , test "allows purchasing multiple with enough funds" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 4 Config.L1 (initialModelWithAvailableFunds <| Currency.Currency 250))
                        (Currency.Currency 230)
            , test "maintains updated costs if the resource has a multiplier" <|
                \_ ->
                    Expect.equal
                        (Inventory.availableFunds <| Inventory.purchaseResource 4 Config.L2 (initialModelWithAvailableFunds <| Currency.Currency 250))
                        (Currency.Currency 175)
            , test "supports chaining multiple purchases" <|
                \_ ->
                    Expect.equal
                        ((initialModelWithAvailableFunds <| Currency.Currency 250)
                            |> Inventory.purchaseResource 2 Config.L2
                            |> Inventory.purchaseResource 2 Config.L2
                            |> Inventory.availableFunds
                        )
                        (Currency.Currency 175)
            ]
        ]
