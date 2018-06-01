module Data.Multipliers.Click
    exposing
        ( Model
        , amount
        , initial
        , setFromInt
        )

import Data.Currency as Currency
import Data.GameConfiguration as Config
import Data.Increasable as Increasable exposing (Increasable)


type alias Model =
    Increasable {}


amount : Model -> Currency.Currency
amount clickMultiplier =
    Currency.Currency <| config.increaseRate ^ (toFloat <| Increasable.totalPurchasedCount clickMultiplier)


initial : Model
initial =
    { basePrice = config.baseCost
    , multiplier = config.increasableMultiplier
    , totalPurchased = Increasable.initialTotalCount
    }


config : Config.ClickMultiplier
config =
    Config.clickMultiplierConfig


setFromInt : Int -> Model -> Model
setFromInt int initialClick =
    Increasable.setTotalPurchased (Increasable.Count int) initialClick
