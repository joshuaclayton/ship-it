module Data.Multipliers.Click
    exposing
        ( Model
        , amount
        , initial
        )

import Data.Currency as Currency
import Data.Increasable as Increasable exposing (Increasable)


type alias Model =
    Increasable {}


amount : Model -> Currency.Currency
amount clickMultiplier =
    Currency.Currency <| 1 * 2 ^ (toFloat <| Increasable.totalPurchasedCount clickMultiplier)


initial : Model
initial =
    { basePrice = Currency.Currency 50
    , multiplier = Increasable.buildMultiplier 3
    , totalPurchased = Increasable.initialTotalCount
    }
