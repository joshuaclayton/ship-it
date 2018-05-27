module View
    exposing
        ( view
        )

import Data.Currency as Currency
import Data.Event exposing (Event(..))
import Data.Expirable as Expirable exposing (Expirable)
import Data.IncomeRate as IncomeRate
import Data.Inventory as Inventory
import Data.Resource as Resource
import FontAwesome as FA
import Html exposing (Html, a, div, h2, h3, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, title)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [] <| List.map viewEvent model.events
        , div [ class "primary-button" ]
            [ generateCurrencyButton model.inventory
            ]
        , div [ class "other-inventory" ]
            [ purchaseMultiplierButtons model.inventory
            ]
        , div [ class "resources-list" ]
            [ resourcesList model.inventory
            ]
        ]


generateCurrencyButton : Inventory.Inventory -> Html Msg
generateCurrencyButton inventory =
    a
        [ onClick GenerateCurrency
        , title <| "Generate " ++ Currency.format (Inventory.clickAmount inventory)
        ]
        [ text "Ship It" ]


purchaseMultiplierButtons : Inventory.Inventory -> Html Msg
purchaseMultiplierButtons inventory =
    div [ class "purchase-multipliers" ]
        [ div [ class "tape" ]
            [ h2 [] [ text "Upgrades" ]
            , ul []
                (purchaseClickMultiplier inventory
                    :: List.map (purchaseMultiplierButton inventory) Resource.levels
                )
            ]
        ]


purchaseMultiplierButton : Inventory.Inventory -> Resource.Level -> Html Msg
purchaseMultiplierButton inventory level =
    li
        [ onClick <| PurchaseResourceMultiplier level
        , title <| "Upgrade your " ++ Inventory.nameFromLevel level ++ " for " ++ Currency.format (Inventory.resourceMultiplierCost inventory level)
        , classList [ ( "disabled", not <| Inventory.canSpend (Inventory.resourceMultiplierCost inventory level) inventory ) ]
        ]
        [ levelToIcon level
        , text " "
        , span [] [ text <| "Purchase a " ++ toString level ++ " multiplier for " ++ Currency.format (Inventory.resourceMultiplierCost inventory level) ]
        ]


purchaseClickMultiplier : Inventory.Inventory -> Html Msg
purchaseClickMultiplier inventory =
    li
        [ onClick PurchaseClickMultiplier
        , title <| "Purchase a click multiplier for " ++ Currency.format (Inventory.clickMultiplierCost inventory)
        , classList [ ( "disabled", not <| Inventory.canSpend (Inventory.clickMultiplierCost inventory) inventory ) ]
        ]
        [ FA.iconWithOptions FA.suitcase FA.Solid [ FA.Size <| FA.Mult 2 ] [ class "multiplier-icon" ]
        , text " "
        , span [] [ text <| "Purchase a click multiplier for " ++ Currency.format (Inventory.clickMultiplierCost inventory) ]
        ]


levelToIcon : Resource.Level -> Html a
levelToIcon level =
    let
        icon =
            case level of
                Resource.L1 ->
                    FA.bicycle

                Resource.L2 ->
                    FA.motorcycle

                Resource.L3 ->
                    FA.car

                Resource.L4 ->
                    FA.plane

                Resource.L5 ->
                    FA.train

                Resource.L6 ->
                    FA.ship

                Resource.L7 ->
                    FA.rocket

                Resource.L8 ->
                    FA.clock
    in
    FA.iconWithOptions icon FA.Solid [ FA.Size <| FA.Mult 2 ] [ class "multiplier-icon" ]


resourcesList : Inventory.Inventory -> Html Msg
resourcesList inventory =
    div [ class "tape" ]
        [ currentIncome inventory
        , resources inventory
        ]


currentIncome : Inventory.Inventory -> Html a
currentIncome inventory =
    div []
        [ h2 [] [ text "Current Funds" ]
        , p
            [ title <| Currency.format (IncomeRate.toCurrency <| Inventory.currentIncomeRate inventory) ++ " per second"
            , class "current-funds"
            ]
            [ text <| Currency.format <| Inventory.availableFunds inventory ]
        ]


resources : Inventory.Inventory -> Html Msg
resources inventory =
    div []
        [ h2 [] [ text "Resources" ]
        , ul []
            (List.map
                (\( level, resource ) -> resourceItem inventory level resource)
                (Inventory.resourcesWithLevels inventory)
            )
        ]


viewEvent : Expirable Event -> Html a
viewEvent expirable =
    case Expirable.value expirable of
        GlobalRateIncrease ->
            div [] [ text "Increase income rate globally" ]

        LocalRateIncrease level ->
            div [] [ text <| "Increase income rate of " ++ Inventory.nameFromLevel level ]


resourceItem : Inventory.Inventory -> Resource.Level -> Resource.Resource -> Html Msg
resourceItem inventory level resource_ =
    li
        [ onClick <| PurchaseResource level
        , classList [ ( "disabled", not <| Inventory.canSpend (Resource.currentPrice resource_) inventory ) ]
        ]
        [ levelToIcon level
        , div [ class "name-and-price" ]
            [ h3 [] [ text <| Resource.name resource_ ]
            , p [ class "current-price" ] [ text <| Currency.format (Resource.currentPrice resource_) ]
            ]
        , p [ class "current-count" ] [ text <| toString (Resource.totalPurchasedCount resource_) ]
        ]
