module Persistence.InventoryDecoder exposing (decodeInventory)

import Data.GameConfiguration as GameConfiguration exposing (Level(..))
import Data.Inventory as Inventory exposing (Inventory(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode


type alias MultiplierCounts =
    { click : Int
    , resources : List ( GameConfiguration.Level, Int )
    }


decodeInventory : Decode.Value -> Inventory
decodeInventory value =
    Decode.decodeValue inventoryDecoder value
        |> Result.withDefault Inventory.initial


inventoryDecoder : Decoder Inventory
inventoryDecoder =
    Decode.map3 Inventory.buildFromInitial
        (Decode.at [ "value", "wallet" ] Decode.float)
        (Decode.at [ "value", "resources" ] resourcesDecoder)
        (Decode.at [ "value", "multipliers" ] multiplierCountsDecoder)


decodeResourceCounts : List ( String, Int ) -> Decoder (List ( GameConfiguration.Level, Int ))
decodeResourceCounts =
    Decode.combine
        << List.map
            (\( levelString, value ) ->
                case GameConfiguration.levelFromString levelString of
                    Ok level ->
                        Decode.succeed ( level, value )

                    Err error ->
                        Decode.fail error
            )


resourcesDecoder : Decoder (List ( GameConfiguration.Level, Int ))
resourcesDecoder =
    Decode.keyValuePairs Decode.int
        |> Decode.andThen decodeResourceCounts


multiplierCountsDecoder : Decoder MultiplierCounts
multiplierCountsDecoder =
    Decode.map2 MultiplierCounts
        (Decode.field "click" Decode.int)
        (Decode.field "resources" resourceMultipliersDecoder)


resourceMultipliersDecoder : Decoder (List ( GameConfiguration.Level, Int ))
resourceMultipliersDecoder =
    Decode.keyValuePairs Decode.int
        |> Decode.andThen decodeResourceCounts
