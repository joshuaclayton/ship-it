module Data.CurrencyTest exposing (..)

import Data.Currency as Currency
import Expect
import Fuzz
import Test exposing (..)


suite : Test
suite =
    describe "Data.Currency"
        [ describe "add"
            [ test "adds two currencies" <|
                \_ ->
                    Expect.equal
                        (Currency.add (Currency.Currency 2) (Currency.Currency 3))
                        (Currency.Currency 5)
            , fuzz (Fuzz.map2 (,) Fuzz.float Fuzz.float) "sums currencies like floats" <|
                \( f1, f2 ) ->
                    Expect.equal
                        (Currency.add (Currency.Currency f1) (Currency.Currency f2))
                        (Currency.Currency <| f1 + f2)
            ]
        , describe "subtract"
            [ test "subtracts the first value from the second value" <|
                \_ ->
                    Expect.equal
                        (Currency.subtract (Currency.Currency 2) (Currency.Currency 3))
                        (Currency.Currency 1)
            , fuzz (Fuzz.map2 (,) Fuzz.float Fuzz.float) "subtracts currencies like floats" <|
                \( f1, f2 ) ->
                    Expect.equal
                        (Currency.subtract (Currency.Currency f1) (Currency.Currency f2))
                        (Currency.Currency <| f2 - f1)
            ]
        , describe "multiply"
            [ test "multiplies the currency by a value" <|
                \_ ->
                    Expect.equal
                        (Currency.multiply (Currency.Currency 2) 2.5)
                        (Currency.Currency 5)
            , fuzz (Fuzz.map2 (,) Fuzz.float Fuzz.float) "multiplies currencies by floats" <|
                \( f1, f2 ) ->
                    Expect.equal
                        (Currency.multiply (Currency.Currency f1) f2)
                        (Currency.Currency <| f1 * f2)
            ]
        , describe "sum"
            [ fuzz (Fuzz.list Fuzz.float) "sums values correctly" <|
                \values ->
                    Expect.equal
                        (Currency.sum <| List.map Currency.Currency values)
                        (Currency.Currency <| List.sum values)
            ]
        ]
