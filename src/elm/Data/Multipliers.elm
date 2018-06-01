module Data.Multipliers
    exposing
        ( Model
        , addLimitedMultiplier
        , clickAmount
        , clickMultiplierCost
        , increaseMultiplierForLevel
        , incrementClickMultiplier
        , incrementResourceMultiplier
        , initial
        , resourceMultiplierCost
        , tick
        )

import Data.Currency as Currency
import Data.Expirable as Expirable
import Data.GameConfiguration as Config
import Data.Increasable as Increasable
import Data.Multipliers.Click as ClickMultiplier
import Data.Multipliers.Limited as LimitedMultiplier
import Data.Multipliers.Resource as ResourceMultiplier


type Model
    = Multipliers ClickMultiplier.Model ResourceMultiplier.Model LimitedMultiplier.Model


initial : Model
initial =
    Multipliers ClickMultiplier.initial ResourceMultiplier.initial LimitedMultiplier.initial


mapClick : (ClickMultiplier.Model -> ClickMultiplier.Model) -> Model -> Model
mapClick f (Multipliers clickMultiplier resourcesMultipliers limitedMultipliers) =
    Multipliers (f clickMultiplier) resourcesMultipliers limitedMultipliers


mapLimited : (LimitedMultiplier.Model -> LimitedMultiplier.Model) -> Model -> Model
mapLimited f (Multipliers clickMultiplier resourcesMultipliers limitedMultipliers) =
    Multipliers clickMultiplier resourcesMultipliers (f limitedMultipliers)


mapResources : (ResourceMultiplier.Model -> ResourceMultiplier.Model) -> Model -> Model
mapResources f (Multipliers clickMultiplier resourcesMultipliers limitedMultipliers) =
    Multipliers clickMultiplier (f resourcesMultipliers) limitedMultipliers


tick : Model -> Model
tick =
    mapLimited Expirable.tickAll


incrementClickMultiplier : Model -> Model
incrementClickMultiplier =
    mapClick Increasable.incrementTotalPurchased


incrementResourceMultiplier : Config.Level -> Model -> Model
incrementResourceMultiplier level =
    mapResources (ResourceMultiplier.incrementTotalPurchased level)


increaseMultiplierForLevel : Model -> Config.Level -> Increasable.Multiplier
increaseMultiplierForLevel (Multipliers _ resourcesMultipliers limitedMultipliers) level =
    Increasable.combineMultipliers
        [ ResourceMultiplier.resourceLevelMultipliers resourcesMultipliers level
        , LimitedMultiplier.resourceLevelMultipliers limitedMultipliers level
        ]


addLimitedMultiplier : LimitedMultiplier.MultiplierType -> Model -> Model
addLimitedMultiplier multiplierType model =
    mapLimited (\xs -> LimitedMultiplier.build multiplierType :: xs) model


clickAmount : Model -> Currency.Currency
clickAmount (Multipliers clickMultiplier _ limitedMultipliers) =
    let
        amountPerClick =
            ClickMultiplier.amount clickMultiplier

        limitedClickMultipliers =
            LimitedMultiplier.clickMultipliers limitedMultipliers
    in
    Currency.multiply amountPerClick (Increasable.multiplierValue <| limitedClickMultipliers)


clickMultiplierCost : Model -> Currency.Currency
clickMultiplierCost (Multipliers clickMultiplier _ _) =
    Increasable.currentPrice clickMultiplier


resourceMultiplierCost : Model -> Config.Level -> Currency.Currency
resourceMultiplierCost (Multipliers _ resourcesMultipliers _) level =
    ResourceMultiplier.currentPrice resourcesMultipliers level
