module Data.Multipliers.Resource
    exposing
        ( Model
        , currentPrice
        , incrementTotalPurchased
        , initial
        , resourceLevelMultipliers
        )

import AllDict exposing (AllDict)
import Data.Currency as Currency
import Data.Increasable as Increasable exposing (Increasable)
import Data.Resource as Resource


type alias Model =
    AllDict Resource.Level ResourceMultiplier String


type alias ResourceMultiplier =
    Increasable {}


increaseMultiplier : ResourceMultiplier -> Increasable.Multiplier
increaseMultiplier model =
    Increasable.buildMultiplier <| 1.03 ^ toFloat (Increasable.totalPurchasedCount model)


incrementTotalPurchased : Resource.Level -> Model -> Model
incrementTotalPurchased level =
    AllDict.update level (Maybe.map Increasable.incrementTotalPurchased)


initial : Model
initial =
    AllDict.fromList toString
        [ build Resource.L1 (Currency.Currency 50)
        , build Resource.L2 (Currency.Currency 500)
        , build Resource.L3 (Currency.Currency 5000)
        , build Resource.L4 (Currency.Currency 50000)
        , build Resource.L5 (Currency.Currency 500000)
        , build Resource.L6 (Currency.Currency 5000000)
        , build Resource.L7 (Currency.Currency 50000000)
        , build Resource.L8 (Currency.Currency 500000000)
        ]


resourceLevelMultipliers : Model -> Resource.Level -> Increasable.Multiplier
resourceLevelMultipliers model level =
    AllDict.get level model
        |> Maybe.map increaseMultiplier
        |> Maybe.withDefault (Increasable.buildMultiplier 1)


build : Resource.Level -> Currency.Currency -> ( Resource.Level, ResourceMultiplier )
build level basePrice =
    ( level
    , { basePrice = basePrice
      , multiplier = Increasable.buildMultiplier 3
      , totalPurchased = Increasable.initialTotalCount
      }
    )


currentPrice : Model -> Resource.Level -> Currency.Currency
currentPrice model level =
    AllDict.get level model
        |> Maybe.map Increasable.currentPrice
        |> Maybe.withDefault Currency.zero
