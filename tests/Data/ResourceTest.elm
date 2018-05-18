module Data.ResourceTest exposing (..)

import Data.Currency as Currency
import Data.Resource as Resource
import Expect
import Test exposing (..)


constantCostResource : Resource.Resource
constantCostResource =
    Resource.build "Constant cost resource" 1 1 (Currency.Currency 5)


doubleCostResource : Resource.Resource
doubleCostResource =
    Resource.build "Double cost resource" 1 2 (Currency.Currency 5)


suite : Test
suite =
    describe "Data.Resource"
        [ describe "purchase"
            [ test "calculates the correct cost when constant" <|
                \_ ->
                    Expect.equal
                        (Resource.transactionCost <| Resource.purchase 5 constantCostResource)
                        (Currency.Currency 25)
            , test "calculates the correct cost when cost doubles" <|
                -- 5 + 10 + 20 + 40 + 80
                \_ ->
                    Expect.equal
                        (Resource.transactionCost <| Resource.purchase 5 doubleCostResource)
                        (Currency.Currency 155)
            , test "calculates the current price after a transaction when the cost is constant" <|
                \_ ->
                    Expect.equal
                        (Resource.currentPrice <| Resource.applyTransaction <| Resource.purchase 5 constantCostResource)
                        (Currency.Currency 5)
            , test "calculates the current price after a transaction when the cost doubles" <|
                \_ ->
                    Expect.equal
                        (Resource.currentPrice <| Resource.applyTransaction <| Resource.purchase 5 doubleCostResource)
                        (Currency.Currency 160)
            , test "calculates the current price with an existing inventory" <|
                \_ ->
                    Expect.equal
                        (Resource.currentPrice <| Resource.applyTransaction <| Resource.purchase 4 <| Resource.applyTransaction <| Resource.purchase 1 doubleCostResource)
                        (Currency.Currency 160)
            , test "calculates the current price with an existing inventory and no new purchase" <|
                \_ ->
                    Expect.equal
                        (Resource.currentPrice <| Resource.applyTransaction <| Resource.purchase 0 <| Resource.applyTransaction <| Resource.purchase 1 doubleCostResource)
                        (Currency.Currency 10)
            , test "disallows purchasing negative amounts" <|
                \_ ->
                    Expect.equal
                        (Resource.currentPrice <| Resource.applyTransaction <| Resource.purchase -2 doubleCostResource)
                        (Currency.Currency 5)
            , test "does not decrement total purchased count when purchasing negative amounts" <|
                \_ ->
                    Expect.equal
                        (Resource.totalPurchasedCount <| Resource.applyTransaction <| Resource.purchase -2 doubleCostResource)
                        0
            ]
        ]
