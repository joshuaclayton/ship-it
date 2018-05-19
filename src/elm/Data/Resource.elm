module Data.Resource
    exposing
        ( Level(..)
        , Resource
        , Transaction
        , applyTransaction
        , build
        , currentPrice
        , purchase
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
        ( purchased, totalPrice ) =
            Increasable.purchase resource (Count count)

        newResource =
            resource
                |> Increasable.increaseTotalPurchased purchased
    in
    Transaction newResource totalPrice (Purchased purchased)


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
