module Data.IncomeRate
    exposing
        ( IncomeRate(..)
        , add
        , build
        , multiply
        , sum
        , toCurrency
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


toCurrency : IncomeRate -> Currency
toCurrency (IncomeRate rate) =
    rate


add : IncomeRate -> IncomeRate -> IncomeRate
add (IncomeRate i1) (IncomeRate i2) =
    IncomeRate <| Currency.add i1 i2


multiply : IncomeRate -> Float -> IncomeRate
multiply (IncomeRate i) f =
    IncomeRate <| Currency.multiply i f


sum : List IncomeRate -> IncomeRate
sum =
    List.foldl add zero
