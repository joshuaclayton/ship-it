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
import LocalStorage exposing (getItem, setItem)
import LocalStorage.SharedTypes exposing (Ports)
import Model exposing (Model, Msg(..))
import Persistence.InventoryDecoder exposing (decodeInventory)
import Persistence.InventoryEncoder exposing (encodeInventory)
import Persistence.Ports exposing (receiveItem)
import Random
import Time


init : Ports Msg -> ( Model, Cmd Msg )
init ports =
    let
        initialModel =
            Model.initial ports
    in
    initialModel ! [ loadInventoryFromLocalStorage initialModel ]


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        eventConfig =
            Inventory.randomEventConfig model.inventory
    in
    Sub.batch
        [ Expirable.expirableSubscription (always DecrementToastMessages)
        , Expirable.expirableSubscription (always TickMultipliers)
        , Expirable.expirableSubscription (always TickEvents)
        , Expirable.expirableSubscription (always TickRecentlyGeneratedCurrency)
        , Time.every Config.updateFrequencyInMs AccrueValue
        , Time.every eventConfig.frequency (always RollForEvents)
        , Time.every (Time.second * 5) (always SetItem)
        , receiveItem UpdatePort
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
            let
                amount =
                    Inventory.clickAmount model.inventory

                newModel =
                    { model
                        | inventory = Inventory.generateCurrency model.inventory
                    }
                        |> Model.trackRecentlyGeneratedCurrency amount
            in
            newModel ! []

        AccrueValue time ->
            let
                multiplierNumerator =
                    case model.lastAccruedTime of
                        Just oldTime ->
                            let
                                timeDifferenceInMs =
                                    time - oldTime

                                outsideThresholdDifference =
                                    timeDifferenceInMs * 1.1 > Config.updateFrequencyInMs
                            in
                            if outsideThresholdDifference then
                                timeDifferenceInMs
                            else
                                Config.updateFrequencyInMs

                        Nothing ->
                            Config.updateFrequencyInMs
            in
            { model
                | inventory = Inventory.accrueValue (multiplierNumerator / 1000) model.inventory
                , lastAccruedTime = Just time
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
            let
                eventConfig =
                    Inventory.randomEventConfig model.inventory
            in
            { model
                | events =
                    Expirable.expiresIn (Expirable.SecondsRemaining eventConfig.eventVisibilityDuration) event :: model.events
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

        TickRecentlyGeneratedCurrency ->
            { model | recentlyGeneratedCurrency = Expirable.tickAll model.recentlyGeneratedCurrency } ! []

        GetItem ->
            model ! [ loadInventoryFromLocalStorage model ]

        SetItem ->
            model ! [ saveInventoryToLocalStorage model ]

        UpdatePort value ->
            { model | inventory = decodeInventory value } ! []


loadInventoryFromLocalStorage : Model -> Cmd Msg
loadInventoryFromLocalStorage { storage } =
    getItem storage "inventory"


saveInventoryToLocalStorage : Model -> Cmd Msg
saveInventoryToLocalStorage { storage, inventory } =
    setItem storage "inventory" (encodeInventory inventory)
