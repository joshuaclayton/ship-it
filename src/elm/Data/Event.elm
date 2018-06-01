module Data.Event exposing (Event(..), optionalRandom, random)

import Data.GameConfiguration as Config
import Random exposing (Generator)
import Random.Extra as Random


type Event
    = GlobalRateIncrease
    | LocalRateIncrease Config.Level


all : List Event
all =
    GlobalRateIncrease
        :: List.map LocalRateIncrease Config.allLevels


totalOdds : Int
totalOdds =
    2


optionalRandom : Generator (Maybe Event)
optionalRandom =
    random
        |> Random.maybe (Random.oneIn totalOdds)


random : Generator Event
random =
    Random.sample all
        |> Random.map (Maybe.withDefault GlobalRateIncrease)
