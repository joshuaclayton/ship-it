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


all : Offset -> List Config.Level -> List Event
all offset levels =
    GlobalRateIncrease offset
        :: List.map (LocalRateIncrease offset) levels


totalOdds : Int
totalOdds =
    4


optionalRandom : List Config.Level -> Generator (Maybe Event)
optionalRandom availableLevels =
    random availableLevels
        |> Random.maybe (Random.oneIn totalOdds)


random : List Config.Level -> Generator Event
random availableLevels =
    randomOffset
        |> Random.andThen
            (\offset ->
                all offset availableLevels
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
