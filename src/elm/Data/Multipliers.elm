module Data.Multipliers
    exposing
        ( Model
        , clickAmount
        , clickMultiplierCost
        , incrementClickMultiplier
        , initial
        )

import Data.Currency as Currency
import Data.Increasable as Increasable
import Data.Multipliers.Click as ClickMultiplier


type Model
    = Multipliers ClickMultiplier.Model


initial : Model
initial =
    Multipliers ClickMultiplier.initial


clickAmount : Model -> Currency.Currency
clickAmount (Multipliers clickMultiplier) =
    ClickMultiplier.amount clickMultiplier


clickMultiplierCost : Model -> Currency.Currency
clickMultiplierCost (Multipliers clickMultiplier) =
    Increasable.currentPrice clickMultiplier


incrementClickMultiplier : Model -> Model
incrementClickMultiplier (Multipliers clickMultiplier) =
    Multipliers (Increasable.incrementTotalPurchased clickMultiplier)
