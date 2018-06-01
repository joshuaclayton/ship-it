module Data.Increasable
    exposing
        ( Count(..)
        , Increasable
        , Multiplier
        , Purchased(..)
        , Total(..)
        , buildMultiplier
        , combineMultipliers
        , currentPrice
        , currentPriceByPurchasedCount
        , increaseTotalPurchased
        , incrementTotalPurchased
        , initialTotalCount
        , mapMultiplier
        , multiplierValue
        , noOp
        , purchase
        , setTotalPurchased
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


noOp : Multiplier
noOp =
    buildMultiplier 1


multiplierValue : Multiplier -> Float
multiplierValue (Multiplier v) =
    v


mapMultiplier : (Float -> Float) -> Multiplier -> Multiplier
mapMultiplier f (Multiplier v) =
    Multiplier <| f v


combineMultipliers : List Multiplier -> Multiplier
combineMultipliers multipliers =
    let
        add n1 n2 =
            n1 + n2
    in
    List.map (\m -> multiplierValue m - 1) multipliers
        |> List.sum
        |> add 1
        |> Multiplier


initialTotalCount : Total Purchased
initialTotalCount =
    Total <| Purchased <| Count 0


setTotalPurchased : Count -> Increasable a -> Increasable a
setTotalPurchased count increasable =
    { increasable | totalPurchased = Total <| Purchased count }


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


purchase : Increasable a -> Count -> ( Count, Total Currency.Currency )
purchase model (Count count) =
    let
        startingCount =
            totalPurchasedCount model

        priceExponents =
            List.reverse <| List.range startingCount (startingCount + count - 1)
    in
    if count > 0 then
        ( Count count
        , List.map (currentPriceByPurchasedCount model) priceExponents
            |> Currency.sum
            |> Total
        )
    else
        ( Count 0
        , Total Currency.zero
        )
