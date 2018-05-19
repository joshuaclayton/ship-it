module Data.Increasable
    exposing
        ( Count(..)
        , Increasable
        , Purchased(..)
        , Total(..)
        , buildMultiplier
        , currentPrice
        , currentPriceByPurchasedCount
        , increaseTotalPurchased
        , incrementTotalPurchased
        , initialTotalCount
        , totalPurchasedCount
        )

import Data.Currency as Currency


type Multiplier
    = Multiplier Float


type Total a
    = Total a


type Purchased
    = Purchased Count


type Count
    = Count Int


type alias Increasable a =
    { a
        | basePrice : Currency.Currency
        , multiplier : Multiplier
        , totalPurchased : Total Purchased
    }


buildMultiplier : Float -> Multiplier
buildMultiplier =
    Multiplier


initialTotalCount : Total Purchased
initialTotalCount =
    Total <| Purchased <| Count 0


increaseTotalPurchased : Count -> Increasable a -> Increasable a
increaseTotalPurchased (Count c) ({ totalPurchased } as model) =
    let
        increment (Total (Purchased (Count v))) =
            Total <| Purchased <| Count <| v + c
    in
    { model | totalPurchased = increment totalPurchased }


incrementTotalPurchased : Increasable a -> Increasable a
incrementTotalPurchased =
    increaseTotalPurchased (Count 1)


currentPrice : Increasable a -> Currency.Currency
currentPrice model =
    currentPriceByPurchasedCount model (totalPurchasedCount model)


totalPurchasedCount : Increasable a -> Int
totalPurchasedCount { totalPurchased } =
    let
        (Total (Purchased (Count count))) =
            totalPurchased
    in
    count


currentPriceByPurchasedCount : Increasable a -> Int -> Currency.Currency
currentPriceByPurchasedCount { basePrice, multiplier } exp =
    let
        (Currency.Currency price) =
            basePrice

        (Multiplier multiplier_) =
            multiplier
    in
    Currency.Currency <| price * multiplier_ ^ toFloat exp
