module Update
    exposing
        ( init
        , subscriptions
        , update
        )

import Data.Currency as Currency
import Data.Expirable as Expirable
import Data.IncomeRate as IncomeRate
import Data.Resource as Resource
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

        GenerateCurrency ->
            { model | availableFunds = Currency.add (Currency.Currency 1) model.availableFunds } ! []

        DecrementToastMessages ->
            { model | toastMessages = Expirable.tickAll model.toastMessages } ! []

        AccrueValue ->
            { model
                | availableFunds = IncomeRate.addToCurrency (IncomeRate.multiply (Resource.totalIncomeRate model.resources) (updateFrequencyInMs / 1000)) model.availableFunds
            }
                ! []

        PurchaseResource resource ->
            let
                transaction =
                    Resource.purchase 1 resource

                newResource =
                    Resource.applyTransaction transaction

                newResources =
                    List.map
                        (\r ->
                            if r == resource then
                                newResource
                            else
                                r
                        )
                        model.resources
            in
            { model
                | resources = newResources
                , availableFunds = Currency.subtract (Resource.transactionCost transaction) model.availableFunds
            }
                ! []
