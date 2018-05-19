module View
    exposing
        ( view
        )

import Data.Inventory as Inventory
import Data.Resource as Resource
import Html exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div []
        [ text "Hello world"
        , button [ onClick GenerateCurrency ] [ text <| "Generate " ++ toString (Inventory.clickAmount model.inventory) ]
        , button [ onClick PurchaseClickMultiplier ] [ text <| "Purchase a multiplier for " ++ toString (Inventory.clickMultiplierCost model.inventory) ]
        , br [] []
        , text <| toString <| Inventory.availableFunds model.inventory
        , br [] []
        , text <| toString model.toastMessages
        , br [] []
        , text <| "Rate per second: " ++ toString (Resource.totalIncomeRate <| Inventory.resources model.inventory)
        , br [] []
        , resourcesList <| Inventory.resourcesWithLevels model.inventory
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
