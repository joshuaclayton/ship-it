module Model
    exposing
        ( Model
        , Msg(..)
        , initial
        )

import Data.Event exposing (Event)
import Data.Expirable exposing (Expirable, SecondsRemaining(..), expiresIn)
import Data.GameConfiguration as Config
import Data.Inventory as Inventory


type alias Model =
    { toastMessages : List (Expirable String)
    , inventory : Inventory.Inventory
    , events : List (Expirable Event)
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
    | AddEvent Event


initial : Model
initial =
    { toastMessages =
        [ expiresIn (SecondsRemaining 5) "Hi there"
        , expiresIn (SecondsRemaining 30) "This goes longer"
        ]
    , inventory = Inventory.initial
    , events = []
    }
