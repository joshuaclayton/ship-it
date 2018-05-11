module Data.Resource
    exposing
        ( Resource
        , Transaction
        , applyTransaction
        , build
        , currentPrice
        , purchase
        , totalIncomeRate
        , transactionCost
        )

import Data.Currency as Currency exposing (Currency(..))
import Data.IncomeRate as IncomeRate exposing (IncomeRate(..))


type alias Resource =
    { name : Name
    , multiplier : Multiplier
    , incomeRate : IncomeRate
    , currentPrice : Current Currency
    , totalPurchased : Total Purchased
    }


type Transaction
    = Transaction Resource (Total Currency) Purchased


type Name
    = Name String


type Current a
    = Current a


type Total a
    = Total a


type Count
    = Count Int


type Purchased
    = Purchased Count


type Multiplier
    = Multiplier Float


build : String -> Float -> Float -> Currency -> Resource
build name incomeRate multiplier startingPrice =
    { name = Name name
    , incomeRate = IncomeRate.build incomeRate
    , multiplier = Multiplier multiplier
    , currentPrice = Current startingPrice
    , totalPurchased = Total <| Purchased <| Count 0
    }


currentPrice : Resource -> Currency
currentPrice { currentPrice } =
    let
        (Current price) =
            currentPrice
    in
    price


totalPurchasedCount : Resource -> Int
totalPurchasedCount { totalPurchased } =
    let
        (Total (Purchased (Count count))) =
            totalPurchased
    in
    count


purchase : Int -> Resource -> Transaction
purchase count resource =
    let
        ( totalPrice, newCurrentPrice ) =
            calculate resource.multiplier (Count count) resource.currentPrice

        newResource =
            { resource | currentPrice = newCurrentPrice }
                |> increasePurchased (Count count)
    in
    Transaction newResource totalPrice (Purchased (Count count))


transactionCost : Transaction -> Currency
transactionCost (Transaction _ (Total price) _) =
    price


applyTransaction : Transaction -> Resource
applyTransaction (Transaction resource _ _) =
    resource


calculate : Multiplier -> Count -> Current Currency -> ( Total Currency, Current Currency )
calculate multiplier (Count count) price =
    let
        calculatePrice (Current (Currency p)) (Multiplier m) exp =
            Currency <| p * m ^ toFloat exp

        priceExponents =
            List.reverse <| List.range 0 (count - 1)
    in
    case List.map (calculatePrice price multiplier) priceExponents of
        [] ->
            ( Total <| Currency.zero, price )

        (newCurrentPrice :: rest) as xs ->
            ( Total <| Currency.sum xs
            , Current <| multiplyCurrency newCurrentPrice multiplier
            )


incomeRatePerSecond : Resource -> IncomeRate
incomeRatePerSecond ({ incomeRate } as resource) =
    IncomeRate.multiply incomeRate (toFloat (totalPurchasedCount resource))


multiplyCurrency : Currency -> Multiplier -> Currency
multiplyCurrency c (Multiplier m) =
    Currency.multiply c m


increasePurchased : Count -> Resource -> Resource
increasePurchased (Count count) resource =
    { resource
        | totalPurchased = Total <| Purchased <| Count <| totalPurchasedCount resource + count
    }


totalIncomeRate : List Resource -> IncomeRate
totalIncomeRate =
    IncomeRate.sum << List.map incomeRatePerSecond
