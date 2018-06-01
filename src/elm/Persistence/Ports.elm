port module Persistence.Ports exposing (ports, receiveItem)

import LocalStorage exposing (makeRealPorts)
import LocalStorage.SharedTypes
    exposing
        ( ClearPort
        , GetItemPort
        , ListKeysPort
        , Ports
        , ReceiveItemPort
        , SetItemPort
        )
import Model exposing (Msg)


port setItem : SetItemPort msg


port getItem : GetItemPort msg


port clear : ClearPort msg


port listKeys : ListKeysPort msg


port receiveItem : ReceiveItemPort msg


ports : Ports Msg
ports =
    makeRealPorts getItem setItem clear listKeys
