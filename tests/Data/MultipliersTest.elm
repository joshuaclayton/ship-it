module Data.MultipliersTest exposing (..)

import Data.Currency as Currency
import Data.Multipliers as Multipliers
import Data.Resource as Resource
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Data.Multipliers"
        [ describe "clickAmount"
            [ test "defaults to 1" <|
                \_ ->
                    Expect.equal
                        (Multipliers.clickAmount Multipliers.initial)
                        (Currency.Currency 1)
            , test "returns the correct value when incremented once" <|
                \_ ->
                    Expect.equal
                        (Multipliers.clickAmount <| Multipliers.incrementClickMultiplier Multipliers.initial)
                        (Currency.Currency 2)
            , test "returns the correct value when incremented multiple times" <|
                \_ ->
                    Expect.equal
                        (Multipliers.clickAmount <| Multipliers.incrementClickMultiplier <| Multipliers.incrementClickMultiplier <| Multipliers.incrementClickMultiplier Multipliers.initial)
                        (Currency.Currency 8)
            ]
        , describe "clickMultiplierCost"
            [ test "defaults to 50" <|
                \_ ->
                    Expect.equal
                        (Multipliers.clickMultiplierCost Multipliers.initial)
                        (Currency.Currency 50)
            , test "returns the correct value when incremented" <|
                \_ ->
                    Expect.equal
                        (Multipliers.clickMultiplierCost <| Multipliers.incrementClickMultiplier Multipliers.initial)
                        (Currency.Currency 150)
            ]
        , describe "resourceMultiplierCost"
            [ test "defaults to 50 for the first level" <|
                \_ ->
                    Expect.equal
                        (Multipliers.resourceMultiplierCost Multipliers.initial Resource.L1)
                        (Currency.Currency 50)
            , test "returns the correct value when incremented" <|
                \_ ->
                    Expect.equal
                        (Multipliers.resourceMultiplierCost (Multipliers.incrementResourceMultiplier Resource.L1 Multipliers.initial) Resource.L1)
                        (Currency.Currency 150)
            ]
        ]
