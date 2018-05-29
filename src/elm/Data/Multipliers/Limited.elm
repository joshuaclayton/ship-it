module Data.Multipliers.Limited
    exposing
        ( Model
        , clickMultipliers
        , initial
        , resourceLevelMultipliers
        )

import Data.Expirable as Expirable
import Data.Increasable as Increasable
import Data.Resource as Resource


type alias Model =
    List (Expirable.Expirable MultiplierType)


initial : Model
initial =
    []


type MultiplierType
    = IncreaseGlobalProduction
    | IncreaseLevelProduction Resource.Level
    | DecreaseGlobalCost
    | DecreaseLevelCost Resource.Level


clickMultipliers : Model -> Increasable.Multiplier
clickMultipliers model =
    List.map (toClickMultiplier << Expirable.value) model
        |> Increasable.combineMultipliers


resourceLevelMultipliers : Model -> Resource.Level -> Increasable.Multiplier
resourceLevelMultipliers model level =
    List.map (toResourceLevelMultiplier level << Expirable.value) model
        |> Increasable.combineMultipliers


toClickMultiplier : MultiplierType -> Increasable.Multiplier
toClickMultiplier multiplierType =
    case multiplierType of
        IncreaseGlobalProduction ->
            Increasable.buildMultiplier 7

        _ ->
            Increasable.buildMultiplier 1


toResourceLevelMultiplier : Resource.Level -> MultiplierType -> Increasable.Multiplier
toResourceLevelMultiplier level multiplierType =
    case multiplierType of
        IncreaseGlobalProduction ->
            Increasable.buildMultiplier 7

        IncreaseLevelProduction l ->
            if l == level then
                Increasable.buildMultiplier 7
            else
                Increasable.noOp

        DecreaseGlobalCost ->
            Increasable.buildMultiplier 0.9

        DecreaseLevelCost l ->
            if l == level then
                Increasable.buildMultiplier 0.77
            else
                Increasable.noOp
