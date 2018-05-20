module Data.Multipliers.Resource
    exposing
        ( Model
        , increaseMultiplier
        , initial
        )

import Data.Currency as Currency
import Data.Increasable as Increasable exposing (Increasable)


type alias Model =
    Increasable {}


increaseMultiplier : Model -> Increasable.Multiplier
increaseMultiplier model =
    Increasable.buildMultiplier <| 1.03 ^ toFloat (Increasable.totalPurchasedCount model)


initial : Currency.Currency -> Model
initial basePrice =
    { basePrice = basePrice
    , multiplier = Increasable.buildMultiplier 3
    , totalPurchased = Increasable.initialTotalCount
    }
