module Data.Inventory
    exposing
        ( Inventory
        , accrueValue
        , availableFunds
        , generateCurrency
        , initial
        , initialWithResources
        , purchaseResource
        , resources
        , resourcesWithLevels
        , setAvailableFunds
        )

import AllDict exposing (AllDict)
import Data.Currency as Currency
import Data.IncomeRate as IncomeRate
import Data.Resource as Resource


type Wallet
    = Wallet Currency.Currency


type alias Resources =
    AllDict Resource.Level Resource.Resource String


type Inventory
    = Inventory Wallet Resources


setAvailableFunds : Currency.Currency -> Inventory -> Inventory
setAvailableFunds newFunds (Inventory _ resources) =
    Inventory (Wallet newFunds) resources


emptyWallet : Wallet
emptyWallet =
    Wallet Currency.zero


initial : Inventory
initial =
    Inventory emptyWallet initialResources


initialWithResources : List ( Resource.Level, Resource.Resource ) -> Inventory
initialWithResources =
    Inventory emptyWallet << AllDict.fromList toString


generateCurrency : Inventory -> Inventory
generateCurrency (Inventory (Wallet funds) resources) =
    let
        newWallet =
            Wallet <| Currency.add (Currency.Currency 1) funds
    in
    Inventory newWallet resources


accrueValue : Float -> Inventory -> Inventory
accrueValue frequency (Inventory (Wallet funds) resources) =
    let
        totalIncomeRate =
            Resource.totalIncomeRate <| AllDict.values resources

        accruedValueTotal =
            IncomeRate.toCurrency <| IncomeRate.multiply totalIncomeRate frequency

        newWallet =
            Wallet <| Currency.add accruedValueTotal funds
    in
    Inventory newWallet resources


availableFunds : Inventory -> Currency.Currency
availableFunds (Inventory (Wallet funds) _) =
    funds


resources : Inventory -> List Resource.Resource
resources (Inventory _ resources) =
    AllDict.values resources


resourcesWithLevels : Inventory -> List ( Resource.Level, Resource.Resource )
resourcesWithLevels (Inventory _ resources) =
    AllDict.toList resources


purchaseResource : Int -> Resource.Level -> Inventory -> Inventory
purchaseResource count level (Inventory wallet resources) =
    case AllDict.get level resources of
        Nothing ->
            Inventory wallet resources

        Just resource ->
            let
                transaction =
                    Resource.purchase count resource

                newResource =
                    Resource.applyTransaction transaction

                newResources =
                    AllDict.update level (always <| Just newResource) resources

                finalCost =
                    Resource.transactionCost transaction
            in
            if wallet |> canPayFor finalCost then
                Inventory (deductFromWallet finalCost wallet) newResources
            else
                Inventory wallet resources


canPayFor : Currency.Currency -> Wallet -> Bool
canPayFor cost wallet =
    Currency.gte (walletValue wallet) cost


walletValue : Wallet -> Currency.Currency
walletValue (Wallet value) =
    value


initialResources : Resources
initialResources =
    AllDict.fromList toString
        [ ( Resource.L1, Resource.build "Cursor" 0.1 1.07 (Currency.Currency 15) )
        , ( Resource.L2, Resource.build "Backpack" 1 1.07 (Currency.Currency 100) )
        , ( Resource.L3, Resource.build "Skateboard" 8 1.07 (Currency.Currency 1100) )
        , ( Resource.L4, Resource.build "Bicycle" 47 1.07 (Currency.Currency 12000) )
        ]


deductFromWallet : Currency.Currency -> Wallet -> Wallet
deductFromWallet amount (Wallet funds) =
    Wallet <| Currency.subtract amount funds
