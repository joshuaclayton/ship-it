module Persistence.InventoryEncoder exposing (encodeInventory)

import Data.Currency as Currency
import Data.GameConfiguration as GameConfiguration
import Data.Increasable as Increasable
import Data.Inventory as Inventory
import Data.Multipliers as Multipliers
import Data.Multipliers.Click as ClickMultipliers
import Data.Multipliers.Resource as ResourceMultipliers
import Data.Resource as Resource
import Json.Encode as Encode exposing (Value)


encodeInventory : Inventory.Inventory -> Value
encodeInventory inventory =
    let
        currency =
            Inventory.extractCurrency inventory

        resources =
            Inventory.extractResources inventory

        multipliers =
            Inventory.extractMultipliers inventory
    in
    Encode.object
        [ ( "wallet", encodeCurrency currency )
        , ( "resources", encodeResources resources )
        , ( "multipliers", encodeMultipliers multipliers )
        ]


encodeResources : GameConfiguration.LevelDict Resource.Resource -> Value
encodeResources =
    Encode.object << GameConfiguration.bimapLevelDict GameConfiguration.levelToString encodeResource


encodeResource : Resource.Resource -> Value
encodeResource =
    Encode.int << Increasable.totalPurchasedCount


encodeMultipliers : Multipliers.Model -> Value
encodeMultipliers multipliers =
    Encode.object
        [ ( "click", encodeClickMultiplier <| Multipliers.extractClick multipliers )
        , ( "resources"
          , encodeResourceMultipliers <|
                Multipliers.extractResources multipliers
          )
        ]


encodeClickMultiplier : ClickMultipliers.Model -> Value
encodeClickMultiplier =
    Encode.int << Increasable.totalPurchasedCount


encodeResourceMultipliers : ResourceMultipliers.Model -> Value
encodeResourceMultipliers =
    Encode.object << GameConfiguration.bimapLevelDict GameConfiguration.levelToString encodeIncreasable


encodeIncreasable : Increasable.Increasable a -> Value
encodeIncreasable =
    Encode.int << Increasable.totalPurchasedCount


encodeCurrency : Currency.Currency -> Encode.Value
encodeCurrency (Currency.Currency c) =
    Encode.float c
