module Data.Multipliers.Limited
    exposing
        ( Model
        , MultiplierType(..)
        , build
        , clickMultipliers
        , initial
        , randomEventsMultiplier
        , resourceLevelMultipliers
        )

import Data.Expirable as Expirable
import Data.GameConfiguration as Config
import Data.Increasable as Increasable


type alias Model =
    List (Expirable.Expirable MultiplierType)


initial : Model
initial =
    []


type MultiplierType
    = IncreaseGlobalProduction
    | IncreaseLevelProduction Config.Level
    | DecreaseGlobalCost
    | DecreaseLevelCost Config.Level
    | ImproveRandomEvents


build : MultiplierType -> Expirable.Expirable MultiplierType
build =
    Expirable.expiresIn (Expirable.SecondsRemaining Config.limitedEventDuration)


clickMultipliers : Model -> Increasable.Multiplier
clickMultipliers model =
    List.map (toClickMultiplier << Expirable.value) model
        |> Increasable.combineMultipliers


resourceLevelMultipliers : Model -> Config.Level -> Increasable.Multiplier
resourceLevelMultipliers model level =
    List.map (toResourceLevelMultiplier level << Expirable.value) model
        |> Increasable.combineMultipliers


randomEventsMultiplier : Model -> Increasable.Multiplier
randomEventsMultiplier model =
    let
        exp =
            List.filter ((==) ImproveRandomEvents << Expirable.value) model
                |> List.length
                |> toFloat
    in
    Increasable.buildMultiplier <| 2 ^ negate exp


toClickMultiplier : MultiplierType -> Increasable.Multiplier
toClickMultiplier multiplierType =
    case multiplierType of
        IncreaseGlobalProduction ->
            Config.limitedIncreasableMultiplier

        _ ->
            Increasable.noOp


toResourceLevelMultiplier : Config.Level -> MultiplierType -> Increasable.Multiplier
toResourceLevelMultiplier level multiplierType =
    case multiplierType of
        IncreaseGlobalProduction ->
            Config.limitedIncreasableMultiplier

        IncreaseLevelProduction l ->
            if l == level then
                Increasable.mapMultiplier ((*) 2) Config.limitedIncreasableMultiplier
            else
                Increasable.noOp

        DecreaseGlobalCost ->
            Config.limitedDecreasableMultiplier

        DecreaseLevelCost l ->
            if l == level then
                Config.limitedDecreasableMultiplier
            else
                Increasable.noOp

        ImproveRandomEvents ->
            Increasable.noOp
