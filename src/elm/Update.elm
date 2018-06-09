module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Data.Event as Event
import Data.Expirable as Expirable
import Data.GameConfiguration as Config
import Data.Inventory as Inventory
import Model exposing (Model, Msg(..))
import Random
import Time


init : ( Model, Cmd Msg )
init =
    Model.initial ! []


subscriptions : Model -> Sub Msg
subscriptions =
    always <|
        Sub.batch
            [ Expirable.expirableSubscription (always DecrementToastMessages)
            , Expirable.expirableSubscription (always TickMultipliers)
            , Expirable.expirableSubscription (always TickEvents)
            , Time.every Config.updateFrequencyInMs (always AccrueValue)
            , Time.every randomEventConfig.frequency (always RollForEvents)
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        DecrementToastMessages ->
            { model | toastMessages = Expirable.tickAll model.toastMessages } ! []

        TickMultipliers ->
            { model | inventory = Inventory.tickMultipliers model.inventory } ! []

        GenerateCurrency ->
            { model | inventory = Inventory.generateCurrency model.inventory } ! []

        AccrueValue ->
            { model
                | inventory = Inventory.accrueValue (Config.updateFrequencyInMs / 1000) model.inventory
            }
                ! []

        PurchaseResource level ->
            { model | inventory = Inventory.purchaseResource 1 level model.inventory } ! []

        PurchaseResourceMultiplier level ->
            { model | inventory = Inventory.purchaseResourceMultiplier model.inventory level } ! []

        PurchaseClickMultiplier ->
            { model | inventory = Inventory.purchaseClickMultiplier model.inventory } ! []

        RollForEvents ->
            model ! [ Random.generate NewEvent (Event.optionalRandom <| Inventory.purchasedLevels model.inventory) ]

        NewEvent Nothing ->
            model ! []

        NewEvent (Just event) ->
            { model
                | events =
                    Expirable.expiresIn (Expirable.SecondsRemaining randomEventConfig.eventVisibilityDuration) event :: model.events
            }
                ! []

        AddEvent event ->
            { model
                | inventory = Inventory.addLimitedMultiplier (Event.toMultiplierType event) model.inventory
                , events = []
            }
                ! []

        TickEvents ->
            { model | events = Expirable.tickAll model.events } ! []


randomEventConfig : Config.RandomEvent
randomEventConfig =
    Config.randomEventConfig
