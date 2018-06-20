module Data.GameConfiguration
    exposing
        ( ClickMultiplier
        , Level(..)
        , LevelDict
        , LimitedMultiplier
        , RandomEvent
        , ResourceMultiplier
        , allLevels
        , buildLevelDict
        , clickMultiplierConfig
        , filterLevelDict
        , increasableMultiplier
        , levelBaseCost
        , levelDictKeys
        , levelIcon
        , levelIncomeRate
        , levelName
        , levelResourceMultiplierCost
        , limitedDecreasableMultiplier
        , limitedEventDuration
        , limitedIncreasableMultiplier
        , randomEventConfig
        , recentlyGeneratedCurrencyWindowInSeconds
        , resourceMultiplierConfig
        , updateFrequencyInMs
        )

import AllDict
import Data.Currency as Currency
import Data.IncomeRate as IncomeRate
import Data.Increasable as Increasable
import FontAwesome as FA
import Time


type Level
    = L1
    | L2
    | L3
    | L4
    | L5
    | L6
    | L7
    | L8


type alias LevelDict a =
    AllDict.AllDict Level a String


filterLevelDict : (Level -> a -> Bool) -> LevelDict a -> LevelDict a
filterLevelDict =
    AllDict.filter


levelDictKeys : LevelDict a -> List Level
levelDictKeys =
    AllDict.keys


updateFrequencyInMs : Time.Time
updateFrequencyInMs =
    50 * Time.millisecond


type alias RandomEvent =
    { frequency : Time.Time
    , eventVisibilityDuration : Time.Time
    }


buildLevelDict : (Level -> a) -> LevelDict a
buildLevelDict constructor =
    AllDict.fromList toString <|
        List.map (\level -> ( level, constructor level )) allLevels


type alias ClickMultiplier =
    { increaseRate : Float
    , baseCost : Currency.Currency
    , increasableMultiplier : Increasable.Multiplier
    }


type alias ResourceMultiplier =
    { increaseRate : Float
    , increasableMultiplier : Increasable.Multiplier
    }


type alias LimitedMultiplier =
    Increasable.Multiplier


allLevels : List Level
allLevels =
    [ L1, L2, L3, L4, L5, L6, L7, L8 ]


increasableMultiplier : Level -> Increasable.Multiplier
increasableMultiplier =
    always <| Increasable.buildMultiplier 1.07


levelIncomeRate : Level -> IncomeRate.IncomeRate
levelIncomeRate level =
    case level of
        L1 ->
            IncomeRate.build 0.1

        L2 ->
            IncomeRate.build 1

        L3 ->
            IncomeRate.build 8

        L4 ->
            IncomeRate.build 47

        L5 ->
            IncomeRate.build 329

        L6 ->
            IncomeRate.build 950

        L7 ->
            IncomeRate.build 4050

        L8 ->
            IncomeRate.build 15000


levelBaseCost : Level -> Currency.Currency
levelBaseCost level =
    case level of
        L1 ->
            Currency.Currency 15

        L2 ->
            Currency.Currency 100

        L3 ->
            Currency.Currency 1100

        L4 ->
            Currency.Currency 12000

        L5 ->
            Currency.Currency 130000

        L6 ->
            Currency.Currency 2000000

        L7 ->
            Currency.Currency 50000000

        L8 ->
            Currency.Currency 200000000


levelResourceMultiplierCost : Level -> Currency.Currency
levelResourceMultiplierCost level =
    case level of
        L1 ->
            Currency.Currency 50

        L2 ->
            Currency.Currency 500

        L3 ->
            Currency.Currency 5000

        L4 ->
            Currency.Currency 50000

        L5 ->
            Currency.Currency 500000

        L6 ->
            Currency.Currency 5000000

        L7 ->
            Currency.Currency 50000000

        L8 ->
            Currency.Currency 500000000


levelName : Level -> String
levelName level =
    case level of
        L1 ->
            "Bike"

        L2 ->
            "Motorcycle"

        L3 ->
            "Car"

        L4 ->
            "Plane"

        L5 ->
            "Train"

        L6 ->
            "Ship"

        L7 ->
            "Rocket"

        L8 ->
            "Time Machine"


levelIcon : Level -> FA.Icon
levelIcon level =
    case level of
        L1 ->
            FA.bicycle

        L2 ->
            FA.motorcycle

        L3 ->
            FA.car

        L4 ->
            FA.plane

        L5 ->
            FA.train

        L6 ->
            FA.ship

        L7 ->
            FA.rocket

        L8 ->
            FA.clock


clickMultiplierConfig : ClickMultiplier
clickMultiplierConfig =
    { increaseRate = 2.4
    , baseCost = Currency.Currency 50
    , increasableMultiplier = Increasable.buildMultiplier 3
    }


limitedIncreasableMultiplier : LimitedMultiplier
limitedIncreasableMultiplier =
    Increasable.buildMultiplier 7


limitedDecreasableMultiplier : LimitedMultiplier
limitedDecreasableMultiplier =
    Increasable.buildMultiplier 0.8


limitedEventDuration : Float
limitedEventDuration =
    75


resourceMultiplierConfig : ResourceMultiplier
resourceMultiplierConfig =
    { increaseRate = 1.03
    , increasableMultiplier = Increasable.buildMultiplier 3
    }


randomEventConfig : RandomEvent
randomEventConfig =
    { frequency = 15 * Time.second
    , eventVisibilityDuration = 10
    }


recentlyGeneratedCurrencyWindowInSeconds : Float
recentlyGeneratedCurrencyWindowInSeconds =
    10
