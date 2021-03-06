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
import Data.GameConfiguration as Config
import Data.IncomeRate as IncomeRate
import Data.Inventory as Inventory
import Expirable exposing (Expirable)
import LocalStorage exposing (LocalStorage)
import LocalStorage.SharedTypes exposing (Ports, Value)
import Time


type alias Model =
    { toastMessages : List (Expirable String)
    , inventory : Inventory.Inventory
    , events : List (Expirable Event)
    , recentlyGeneratedCurrency : List (Expirable Currency.Currency)
    , storage : LocalStorage Msg
    , lastAccruedTime : Maybe Time.Time
    }


type Msg
    = NoOp
    | DecrementToastMessages Time.Time
    | TickMultipliers Time.Time
    | PurchaseResource Config.Level
    | PurchaseResourceMultiplier Config.Level
    | PurchaseClickMultiplier
    | AccrueValue Time.Time
    | GenerateCurrency
    | RollForEvents
    | NewEvent (Maybe Event)
    | TickEvents Time.Time
    | TickRecentlyGeneratedCurrency Time.Time
    | AddEvent Event
    | SetItem
    | GetItem
    | UpdatePort Value


initial : Ports Msg -> Model
initial ports =
    { toastMessages =
        [ Expirable.build (Expirable.seconds 5) "Hi there"
        , Expirable.build (Expirable.seconds 30) "This goes longer"
        ]
    , inventory = Inventory.initial
    , events = []
    , recentlyGeneratedCurrency = []
    , storage = LocalStorage.make ports ""
    , lastAccruedTime = Nothing
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
            Expirable.build (Expirable.seconds Config.recentlyGeneratedCurrencyWindowInSeconds) amount
    in
    { model | recentlyGeneratedCurrency = generatedCurrency :: model.recentlyGeneratedCurrency }


clickedIncomeRatePerSecond : Model -> IncomeRate.IncomeRate
clickedIncomeRatePerSecond { recentlyGeneratedCurrency } =
    List.map (IncomeRate.IncomeRate << Expirable.value) recentlyGeneratedCurrency
        |> IncomeRate.sum
        |> flip IncomeRate.multiply (1 / Config.recentlyGeneratedCurrencyWindowInSeconds)
