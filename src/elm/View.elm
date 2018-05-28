module View
    exposing
        ( view
        )

import Data.Event exposing (Event(..))
import Data.Expirable as Expirable exposing (Expirable)
import Data.Inventory as Inventory
import Data.Resource as Resource
import Html exposing (Html, br, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ div [] <| List.map viewEvent model.events
        , text "Hello world"
        , button [ onClick GenerateCurrency ] [ text <| "Generate " ++ toString (Inventory.clickAmount model.inventory) ]
        , button [ onClick PurchaseClickMultiplier ] [ text <| "Purchase a multiplier for " ++ toString (Inventory.clickMultiplierCost model.inventory) ]
        , br [] []
        , button [ onClick <| PurchaseResourceMultiplier Resource.L1 ] [ text <| "Purchase a L1 multiplier for " ++ toString (Inventory.resourceMultiplierCost model.inventory Resource.L1) ]
        , button [ onClick <| PurchaseResourceMultiplier Resource.L2 ] [ text <| "Purchase a L2 multiplier for " ++ toString (Inventory.resourceMultiplierCost model.inventory Resource.L2) ]
        , button [ onClick <| PurchaseResourceMultiplier Resource.L3 ] [ text <| "Purchase a L3 multiplier for " ++ toString (Inventory.resourceMultiplierCost model.inventory Resource.L3) ]
        , button [ onClick <| PurchaseResourceMultiplier Resource.L4 ] [ text <| "Purchase a L4 multiplier for " ++ toString (Inventory.resourceMultiplierCost model.inventory Resource.L4) ]
        , br [] []
        , text <| toString <| Inventory.availableFunds model.inventory
        , br [] []
        , text <| toString model.toastMessages
        , br [] []
        , text <| "Rate per second: " ++ toString (Inventory.currentIncomeRate model.inventory)
        , br [] []
        , resourcesList <| Inventory.resourcesWithLevels model.inventory
        , text <| toString <| model.inventory
        ]


resourcesList : List ( Resource.Level, Resource.Resource ) -> Html Msg
resourcesList resources =
    ul []
        (List.map
            (\( level, resource ) ->
                li []
                    [ button [ onClick <| PurchaseResource level ] [ text <| "Buy " ++ toString resource.name ++ " for " ++ toString (Resource.currentPrice resource) ]
                    ]
            )
            resources
        )


viewEvent : Expirable Event -> Html a
viewEvent expirable =
    case Expirable.value expirable of
        GlobalRateIncrease ->
            div [] [ text "Increase income rate globally" ]

        LocalRateIncrease lvl ->
            div [] [ text <| "Increase income rate of " ++ toString lvl ]
