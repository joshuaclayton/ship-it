module Model
    exposing
        ( Model
        , Msg(..)
        , initial
        )

import Data.Expirable exposing (Expirable, SecondsRemaining(..), expiresIn)
import Data.Inventory as Inventory
import Data.Resource as Resource


type alias Model =
    { toastMessages : List (Expirable String)
    , inventory : Inventory.Inventory
    }


type Msg
    = NoOp
    | DecrementToastMessages
    | PurchaseResource Resource.Level
    | AccrueValue
    | GenerateCurrency


initial : Model
initial =
    { toastMessages =
        [ expiresIn (SecondsRemaining 5) "Hi there"
        , expiresIn (SecondsRemaining 30) "This goes longer"
        ]
    , inventory = Inventory.initial
    }
