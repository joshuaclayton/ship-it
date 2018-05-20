module Data.Multipliers
    exposing
        ( Model
        , clickAmount
        , clickMultiplierCost
        , increaseMultiplierForLevel
        , incrementClickMultiplier
        , incrementResourceMultiplier
        , initial
        , resourceMultiplierCost
        )

import AllDict exposing (AllDict)
import Data.Currency as Currency
import Data.Increasable as Increasable
import Data.Multipliers.Click as ClickMultiplier
import Data.Multipliers.Resource as ResourceMultiplier
import Data.Resource as Resource


type Model
    = Multipliers ClickMultiplier.Model ResourcesMultipliers


type alias ResourcesMultipliers =
    AllDict Resource.Level ResourceMultiplier.Model String


initial : Model
initial =
    Multipliers ClickMultiplier.initial initialResourcesMultipliers


increaseMultiplierForLevel : Model -> Resource.Level -> Increasable.Multiplier
increaseMultiplierForLevel (Multipliers _ resourcesMultipliers) level =
    AllDict.get level resourcesMultipliers
        |> Maybe.map ResourceMultiplier.increaseMultiplier
        |> Maybe.withDefault (Increasable.buildMultiplier 1)


clickAmount : Model -> Currency.Currency
clickAmount (Multipliers clickMultiplier _) =
    ClickMultiplier.amount clickMultiplier


clickMultiplierCost : Model -> Currency.Currency
clickMultiplierCost (Multipliers clickMultiplier _) =
    Increasable.currentPrice clickMultiplier


resourceMultiplierCost : Model -> Resource.Level -> Currency.Currency
resourceMultiplierCost (Multipliers _ resourcesMultipliers) level =
    AllDict.get level resourcesMultipliers
        |> Maybe.map Increasable.currentPrice
        |> Maybe.withDefault Currency.zero


incrementClickMultiplier : Model -> Model
incrementClickMultiplier (Multipliers clickMultiplier resourcesMultipliers) =
    Multipliers (Increasable.incrementTotalPurchased clickMultiplier) resourcesMultipliers


incrementResourceMultiplier : Model -> Resource.Level -> Model
incrementResourceMultiplier (Multipliers clickMultiplier resourcesMultipliers) level =
    Multipliers clickMultiplier (AllDict.update level (Maybe.map Increasable.incrementTotalPurchased) resourcesMultipliers)


initialResourcesMultipliers : ResourcesMultipliers
initialResourcesMultipliers =
    AllDict.fromList toString
        [ ( Resource.L1, ResourceMultiplier.initial (Currency.Currency 50) )
        , ( Resource.L2, ResourceMultiplier.initial (Currency.Currency 500) )
        , ( Resource.L3, ResourceMultiplier.initial (Currency.Currency 5000) )
        , ( Resource.L4, ResourceMultiplier.initial (Currency.Currency 50000) )
        ]
