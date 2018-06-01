module Data.Multipliers.Resource
    exposing
        ( Model
        , currentPrice
        , incrementTotalPurchased
        , initial
        , resourceLevelMultipliers
        )

import AllDict
import Data.Currency as Currency
import Data.GameConfiguration as Config
import Data.Increasable as Increasable exposing (Increasable)


type alias Model =
    Config.LevelDict ResourceMultiplier


type alias ResourceMultiplier =
    Increasable {}


increaseMultiplier : ResourceMultiplier -> Increasable.Multiplier
increaseMultiplier model =
    Increasable.buildMultiplier <| config.increaseRate ^ toFloat (Increasable.totalPurchasedCount model)


incrementTotalPurchased : Config.Level -> Model -> Model
incrementTotalPurchased level =
    AllDict.update level (Maybe.map Increasable.incrementTotalPurchased)


initial : Model
initial =
    Config.buildLevelDict
        (build << Config.levelResourceMultiplierCost)


resourceLevelMultipliers : Model -> Config.Level -> Increasable.Multiplier
resourceLevelMultipliers model level =
    AllDict.get level model
        |> Maybe.map increaseMultiplier
        |> Maybe.withDefault (Increasable.buildMultiplier 1)


build : Currency.Currency -> ResourceMultiplier
build basePrice =
    { basePrice = basePrice
    , multiplier = config.increasableMultiplier
    , totalPurchased = Increasable.initialTotalCount
    }


currentPrice : Model -> Config.Level -> Currency.Currency
currentPrice model level =
    AllDict.get level model
        |> Maybe.map Increasable.currentPrice
        |> Maybe.withDefault Currency.zero


config : Config.ResourceMultiplier
config =
    Config.resourceMultiplierConfig
