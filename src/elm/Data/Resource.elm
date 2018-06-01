module Data.Resource
    exposing
        ( Resource
        , Transaction
        , applyTransaction
        , build
        , currentPrice
        , name
        , purchase
        , totalIncomeRate
        , totalPurchasedCount
        , transactionCost
        , withIncomeRateMultiplier
        )

import Data.Currency as Currency
import Data.IncomeRate as IncomeRate exposing (IncomeRate(..))
import Data.Increasable as Increasable exposing (Count(..), Increasable, Purchased(..), Total(..))


type alias Resource =
    Increasable
        { name : Name
        , incomeRate : IncomeRate
        }


type Transaction
    = Transaction Resource (Total Currency.Currency) Purchased


type Name
    = Name String


build : String -> IncomeRate.IncomeRate -> Increasable.Multiplier -> Currency.Currency -> Resource
build name incomeRate multiplier startingPrice =
    { name = Name name
    , incomeRate = incomeRate
    , multiplier = multiplier
    , basePrice = startingPrice
    , totalPurchased = Increasable.initialTotalCount
    }


currentPrice : Resource -> Currency.Currency
currentPrice =
    Increasable.currentPrice


name : Resource -> String
name resource =
    let
        (Name value) =
            resource.name
    in
    value


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


transactionCost : Transaction -> Currency.Currency
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


withIncomeRateMultiplier : Resource -> Increasable.Multiplier -> Resource
withIncomeRateMultiplier ({ incomeRate } as resource) multiplier =
    { resource | incomeRate = IncomeRate.multiply incomeRate (Increasable.multiplierValue multiplier) }
