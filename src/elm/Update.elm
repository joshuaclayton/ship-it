module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Data.Expirable as Expirable
import Data.Inventory as Inventory
import Model exposing (Model, Msg(..))
import Time


init : ( Model, Cmd Msg )
init =
    Model.initial ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Expirable.expirableSubscription (always DecrementToastMessages)
        , Time.every (updateFrequencyInMs * Time.millisecond) (always AccrueValue)
        ]


updateFrequencyInMs : Float
updateFrequencyInMs =
    50


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        DecrementToastMessages ->
            { model | toastMessages = Expirable.tickAll model.toastMessages } ! []

        GenerateCurrency ->
            { model | inventory = Inventory.generateCurrency model.inventory } ! []

        AccrueValue ->
            { model
                | inventory = Inventory.accrueValue (updateFrequencyInMs / 1000) model.inventory
            }
                ! []

        PurchaseResource level ->
            { model | inventory = Inventory.purchaseResource 1 level model.inventory } ! []

        PurchaseClickMultiplier ->
            { model | inventory = Inventory.purchaseClickMultiplier model.inventory } ! []
