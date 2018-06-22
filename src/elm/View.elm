module View
    exposing
        ( view
        )

import Data.Currency as Currency
import Data.Event exposing (Event(..), Offset(..))
import Data.Expirable as Expirable exposing (Expirable)
import Data.GameConfiguration as Config
import Data.IncomeRate as IncomeRate
import Data.Inventory as Inventory
import Data.Multipliers.Limited as LimitedMultiplier
import Data.Resource as Resource
import FontAwesome as FA
import Html exposing (Html, a, div, h2, h3, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, style, title)
import Html.Events exposing (onClick)
import Model exposing (Model, Msg(..))
import Svg
import Svg.Attributes as Svg


view : Model -> Html Msg
view model =
    div []
        [ div [ class "events-container" ] <| List.map viewEvent model.events
        , div [ class "container" ]
            [ div [ class "primary-button" ]
                [ generateCurrencyButton model.inventory
                ]
            , div [ class "other-inventory" ]
                [ purchaseMultiplierButtons model.inventory
                ]
            , div [ class "resources-list" ]
                [ resourcesList model
                ]
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
                    :: List.map (purchaseMultiplierButton inventory) Config.allLevels
                )
            ]
        ]


purchaseMultiplierButton : Inventory.Inventory -> Config.Level -> Html Msg
purchaseMultiplierButton inventory level =
    li
        [ onClick <| PurchaseResourceMultiplier level
        , title <| "Upgrade your " ++ Config.levelName level ++ " for " ++ Currency.format (Inventory.resourceMultiplierCost inventory level)
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
        [ suitcaseIcon
        , text " "
        , span [] [ text <| "Purchase a click multiplier for " ++ Currency.format (Inventory.clickMultiplierCost inventory) ]
        ]


levelToIcon : Config.Level -> Html a
levelToIcon level =
    let
        icon =
            Config.levelIcon level
    in
    FA.iconWithOptions icon FA.Solid [ FA.Size <| FA.Mult 2 ] [ class "multiplier-icon" ]


resourcesList : Model -> Html Msg
resourcesList ({ inventory } as model) =
    div [ class "tape" ]
        [ currentLimitedMultipliers inventory
        , currentIncome model
        , resources inventory
        ]


currentIncome : Model -> Html a
currentIncome ({ inventory } as model) =
    let
        currentTotalIncomeRate =
            IncomeRate.toCurrency <| Model.currentIncomeRate model
    in
    div []
        [ h2 [] [ text "Current Funds" ]
        , p [ class "current-funds" ]
            [ text <| Currency.format <| Inventory.availableFunds inventory ]
        , p [ class "income-rate" ]
            [ text <| Currency.format currentTotalIncomeRate ++ "/s" ]
        ]


resources : Inventory.Inventory -> Html Msg
resources inventory =
    div []
        [ h2 [] [ text "Resources" ]
        , ul [ class "purchasable-resources" ]
            (List.map
                (\( level, resource ) -> resourceItem inventory level resource)
                (Inventory.resourcesWithLevels inventory)
            )
        ]


viewEvent : Expirable Event -> Html Msg
viewEvent expirable =
    let
        event =
            Expirable.value expirable
    in
    case event of
        GlobalRateIncrease offset ->
            a
                [ class "event"
                , offsetToStyle offset
                , title "Increase income rate globally"
                , onClick <| AddEvent event
                ]
                [ suitcaseIcon ]

        LocalRateIncrease offset level ->
            a
                [ class "event"
                , offsetToStyle offset
                , title <| "Increase income rate of " ++ Config.levelName level
                , onClick <| AddEvent event
                ]
                [ levelToIcon level ]

        ImprovedRandomEvents offset ->
            a
                [ class "event"
                , offsetToStyle offset
                , title "Improve random events"
                , onClick <| AddEvent event
                ]
                [ randomEventIcon ]


offsetToStyle : Offset -> Html.Attribute a
offsetToStyle (Offset xpos ypos) =
    style [ ( "top", toString ypos ++ "vh" ), ( "left", toString xpos ++ "vw" ) ]


resourceItem : Inventory.Inventory -> Config.Level -> Resource.Resource -> Html Msg
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


suitcaseIcon : Html a
suitcaseIcon =
    FA.iconWithOptions FA.suitcase FA.Solid [ FA.Size <| FA.Mult 2 ] [ class "multiplier-icon" ]


randomEventIcon : Html a
randomEventIcon =
    FA.iconWithOptions FA.tachometerAlt FA.Solid [ FA.Size <| FA.Mult 2 ] [ class "multiplier-icon" ]


currentLimitedMultipliers : Inventory.Inventory -> Html a
currentLimitedMultipliers inventory =
    ul [ class "active-limited-multipliers" ]
        (List.map (\multiplier -> li [] [ limitedMultipler multiplier ])
            (Inventory.currentLimitedMultipliers inventory)
        )


limitedMultipler : Expirable.Expirable LimitedMultiplier.MultiplierType -> Html a
limitedMultipler expirableMultiplier =
    let
        icon =
            case Expirable.value expirableMultiplier of
                LimitedMultiplier.IncreaseGlobalProduction ->
                    suitcaseIcon

                LimitedMultiplier.IncreaseLevelProduction level ->
                    levelToIcon level

                LimitedMultiplier.DecreaseGlobalCost ->
                    suitcaseIcon

                LimitedMultiplier.DecreaseLevelCost level ->
                    levelToIcon level

                LimitedMultiplier.ImproveRandomEvents ->
                    randomEventIcon

        remainingPercentage =
            round <| Expirable.percentComplete expirableMultiplier * 100

        dasharray =
            toString remainingPercentage
                ++ " "
                ++ (toString <| 100 - remainingPercentage)
    in
    span [ class "icon-with-remaining-time" ]
        [ icon
        , Svg.svg [ Svg.class "countdown", Svg.viewBox "0 0 42 42" ]
            [ Svg.circle [ Svg.class "used", Svg.cx "21", Svg.cy "21", Svg.r "15" ] []
            , Svg.circle [ Svg.class "remaining", Svg.cx "21", Svg.cy "21", Svg.r "15", Svg.strokeDasharray dasharray ] []
            ]
        ]
