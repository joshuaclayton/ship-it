module Model
    exposing
        ( Model
        , Msg(..)
        , currentIncomeRate
        , initial
        , trackRecentlyGeneratedCurrency
        )

import Data.Currency as Currency
import Data.Event exposing (Event)
import Data.Expirable as Expirable exposing (Expirable, SecondsRemaining(..), expiresIn)
import Data.GameConfiguration as Config
import Data.IncomeRate as IncomeRate
import Data.Inventory as Inventory


type alias Model =
    { toastMessages : List (Expirable String)
    , inventory : Inventory.Inventory
    , events : List (Expirable Event)
    , recentlyGeneratedCurrency : List (Expirable Currency.Currency)
    }


type Msg
    = NoOp
    | DecrementToastMessages
    | TickMultipliers
    | PurchaseResource Config.Level
    | PurchaseResourceMultiplier Config.Level
    | PurchaseClickMultiplier
    | AccrueValue
    | GenerateCurrency
    | RollForEvents
    | NewEvent (Maybe Event)
    | TickEvents
    | TickRecentlyGeneratedCurrency
    | AddEvent Event


initial : Model
initial =
    { toastMessages =
        [ expiresIn (SecondsRemaining 5) "Hi there"
        , expiresIn (SecondsRemaining 30) "This goes longer"
        ]
    , inventory = Inventory.initial
    , events = []
    , recentlyGeneratedCurrency = []
    }


currentIncomeRate : Model -> IncomeRate.IncomeRate
currentIncomeRate model =
    IncomeRate.add
        (Inventory.currentIncomeRate model.inventory)
        (clickedIncomeRatePerSecond model)


trackRecentlyGeneratedCurrency : Currency.Currency -> Model -> Model
trackRecentlyGeneratedCurrency amount model =
    let
        generatedCurrency =
            Expirable.expiresIn (Expirable.SecondsRemaining Config.recentlyGeneratedCurrencyWindowInSeconds) amount
    in
    { model | recentlyGeneratedCurrency = generatedCurrency :: model.recentlyGeneratedCurrency }


clickedIncomeRatePerSecond : Model -> IncomeRate.IncomeRate
clickedIncomeRatePerSecond { recentlyGeneratedCurrency } =
    List.map (IncomeRate.IncomeRate << Expirable.value) recentlyGeneratedCurrency
        |> IncomeRate.sum
        |> flip IncomeRate.multiply (1 / Config.recentlyGeneratedCurrencyWindowInSeconds)
