module Model
    exposing
        ( Model
        , Msg(..)
        , initial
        )

import Data.Expirable exposing (Expirable, SecondsRemaining(..), expiresIn)


type alias Model =
    { toastMessages : List (Expirable String) }


type Msg
    = NoOp
    | DecrementToastMessages


initial : Model
initial =
    { toastMessages = [ expiresIn (SecondsRemaining 5) "Hi there", expiresIn (SecondsRemaining 30) "This goes longer" ] }
