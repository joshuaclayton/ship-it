module View
    exposing
        ( view
        )

import Data.Inventory as Inventory
import Data.Resource as Resource
import Html exposing (..)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(GenerateCurrency, PurchaseResource))


view : Model -> Html Msg
view model =
    div []
        [ text "Hello world"
        , button [ onClick GenerateCurrency ] [ text "Make a thing" ]
        , br [] []
        , text <| toString <| Inventory.availableFunds model.inventory
        , br [] []
        , text <| toString model.toastMessages
        , br [] []
        , text <| "Rate per second: " ++ toString (Resource.totalIncomeRate <| Inventory.resources model.inventory)
        , br [] []
        , resourcesList <| Inventory.resources model.inventory
        ]


resourcesList : List Resource.Resource -> Html Msg
resourcesList resources =
    ul []
        (List.map
            (\resource ->
                li []
                    [ button [ onClick <| PurchaseResource resource ] [ text <| "Buy " ++ toString resource.name ++ " for " ++ toString (Resource.currentPrice resource) ]
                    ]
            )
            resources
        )
