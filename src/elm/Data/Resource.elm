module Data.Resource
    exposing
        ( Level(..)
        , Resource
        , Transaction
        , applyTransaction
        , build
        , currentPrice
        , purchase
        , replace
        , totalIncomeRate
        , totalPurchasedCount
        , transactionCost
        )

import Data.Currency as Currency exposing (Currency(..))
import Data.IncomeRate as IncomeRate exposing (IncomeRate(..))


type alias Resource =
    { name : Name
    , multiplier : Multiplier
    , incomeRate : IncomeRate
    , basePrice : Currency
    , totalPurchased : Total Purchased
    }


type Level
    = L1
    | L2
    | L3
    | L4


type Transaction
    = Transaction Resource (Total Currency) Purchased


type Name
    = Name String


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
    , basePrice = startingPrice
    , totalPurchased = Total <| Purchased <| Count 0
    }


currentPrice : Resource -> Currency
currentPrice resource =
    currentPriceByPurchasedCount resource (totalPurchasedCount resource)


currentPriceByPurchasedCount : Resource -> Int -> Currency
currentPriceByPurchasedCount { basePrice, multiplier } exp =
    let
        (Currency price) =
            basePrice

        (Multiplier multiplier_) =
            multiplier
    in
    Currency <| price * multiplier_ ^ toFloat exp


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
        startingCount =
            totalPurchasedCount resource

        priceExponents =
            List.reverse <| List.range startingCount (startingCount + count - 1)

        totalPrice =
            List.map (currentPriceByPurchasedCount resource) priceExponents
                |> Currency.sum
                |> Total

        newResource =
            resource
                |> increasePurchased (Count count)
    in
    if count > 0 then
        Transaction newResource totalPrice (Purchased (Count count))
    else
        Transaction resource (Total Currency.zero) (Purchased (Count 0))


transactionCost : Transaction -> Currency
transactionCost (Transaction _ (Total price) _) =
    price


applyTransaction : Transaction -> Resource
applyTransaction (Transaction resource _ _) =
    resource


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


replace : Resource -> Resource -> Resource -> Resource
replace oldResource newResource r =
    if r == oldResource then
        newResource
    else
        r
