module Data.IncomeRate
    exposing
        ( IncomeRate(..)
        , add
        , addToCurrency
        , build
        , multiply
        , sum
        , zero
        )

import Data.Currency as Currency exposing (Currency)


type IncomeRate
    = IncomeRate Currency


build : Float -> IncomeRate
build =
    IncomeRate << Currency.Currency


zero : IncomeRate
zero =
    IncomeRate Currency.zero


add : IncomeRate -> IncomeRate -> IncomeRate
add (IncomeRate i1) (IncomeRate i2) =
    IncomeRate <| Currency.add i1 i2


addToCurrency : IncomeRate -> Currency -> Currency
addToCurrency (IncomeRate rate) price =
    Currency.add rate price


multiply : IncomeRate -> Float -> IncomeRate
multiply (IncomeRate i) f =
    IncomeRate <| Currency.multiply i f


sum : List IncomeRate -> IncomeRate
sum =
    List.foldl add zero
