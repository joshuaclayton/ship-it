module Model
    exposing
        ( Model
        , Msg(..)
        , initial
        , purchaseResource
        )

import Data.Currency as Currency exposing (Currency(..))
import Data.Expirable exposing (Expirable, SecondsRemaining(..), expiresIn)
import Data.Resource as Resource


type alias Model =
    { toastMessages : List (Expirable String)
    , availableFunds : Currency
    , resources : List Resource.Resource
    }


type Msg
    = NoOp
    | DecrementToastMessages
    | PurchaseResource Resource.Resource
    | AccrueValue
    | GenerateCurrency


initial : Model
initial =
    { toastMessages =
        [ expiresIn (SecondsRemaining 5) "Hi there"
        , expiresIn (SecondsRemaining 30) "This goes longer"
        ]
    , availableFunds = Currency.zero
    , resources =
        [ Resource.build "Cursor" 0.1 1.07 (Currency 15)
        , Resource.build "Backpack" 1 1.07 (Currency 100)
        , Resource.build "Skateboard" 8 1.07 (Currency 1100)
        , Resource.build "Bicycle" 47 1.07 (Currency 12000)
        ]
    }


purchaseResource : Int -> Resource.Resource -> Model -> Model
purchaseResource count resource model =
    let
        transaction =
            Resource.purchase count resource

        newResource =
            Resource.applyTransaction transaction

        newResources =
            List.map (Resource.replace resource newResource) model.resources

        finalCost =
            Resource.transactionCost transaction
    in
    if model |> canPayFor finalCost then
        { model
            | resources = newResources
            , availableFunds = Currency.subtract finalCost model.availableFunds
        }
    else
        model


canPayFor : Currency -> Model -> Bool
canPayFor cost { availableFunds } =
    Currency.gte availableFunds cost
