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
import Data.Increasable as Increasable exposing (Count(..), Increasable, Purchased(..), Total(..))


type alias Resource =
    Increasable
        { name : Name
        , incomeRate : IncomeRate
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


build : String -> Float -> Float -> Currency -> Resource
build name incomeRate multiplier startingPrice =
    { name = Name name
    , incomeRate = IncomeRate.build incomeRate
    , multiplier = Increasable.buildMultiplier multiplier
    , basePrice = startingPrice
    , totalPurchased = Increasable.initialTotalCount
    }


currentPrice : Resource -> Currency
currentPrice =
    Increasable.currentPrice


totalPurchasedCount : Resource -> Int
totalPurchasedCount =
    Increasable.totalPurchasedCount


purchase : Int -> Resource -> Transaction
purchase count resource =
    let
        startingCount =
            totalPurchasedCount resource

        priceExponents =
            List.reverse <| List.range startingCount (startingCount + count - 1)

        totalPrice =
            List.map (Increasable.currentPriceByPurchasedCount resource) priceExponents
                |> Currency.sum
                |> Total

        newResource =
            resource
                |> Increasable.increaseTotalPurchased (Count count)
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


totalIncomeRate : List Resource -> IncomeRate
totalIncomeRate =
    IncomeRate.sum << List.map incomeRatePerSecond


replace : Resource -> Resource -> Resource -> Resource
replace oldResource newResource r =
    if r == oldResource then
        newResource
    else
        r
