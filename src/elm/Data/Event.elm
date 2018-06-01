module Data.Event
    exposing
        ( Event(..)
        , Offset(..)
        , optionalRandom
        , random
        , toMultiplierType
        )

import Data.GameConfiguration as Config
import Data.Multipliers.Limited as Multipliers
import Random exposing (Generator)
import Random.Extra as Random


type Event
    = GlobalRateIncrease Offset
    | LocalRateIncrease Offset Config.Level


type Offset
    = Offset Int Int


toMultiplierType : Event -> Multipliers.MultiplierType
toMultiplierType event =
    case event of
        GlobalRateIncrease _ ->
            Multipliers.IncreaseGlobalProduction

        LocalRateIncrease _ level ->
            Multipliers.IncreaseLevelProduction level


all : Offset -> List Event
all offset =
    GlobalRateIncrease offset
        :: List.map (LocalRateIncrease offset) Config.allLevels


totalOdds : Int
totalOdds =
    2


optionalRandom : Generator (Maybe Event)
optionalRandom =
    random
        |> Random.maybe (Random.oneIn totalOdds)


random : Generator Event
random =
    randomOffset
        |> Random.andThen
            (\offset ->
                all offset
                    |> Random.sample
                    |> Random.map (Maybe.withDefault <| GlobalRateIncrease offset)
            )


randomOffset : Generator Offset
randomOffset =
    let
        randomOffsetValue =
            Random.sample (List.range 10 85)
                |> Random.map (Maybe.withDefault 50)
    in
    Random.map2 Offset randomOffsetValue randomOffsetValue
