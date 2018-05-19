module Data.Inventory
    exposing
        ( Inventory
        , accrueValue
        , availableFunds
        , clickAmount
        , clickMultiplierCost
        , generateCurrency
        , initial
        , initialWithResources
        , purchaseClickMultiplier
        , purchaseResource
        , resources
        , resourcesWithLevels
        , setAvailableFunds
        )

import AllDict exposing (AllDict)
import Data.Currency as Currency
import Data.IncomeRate as IncomeRate
import Data.Multipliers as Multipliers
import Data.Resource as Resource
import Data.Wallet as Wallet exposing (Wallet)


type alias Resources =
    AllDict Resource.Level Resource.Resource String


type Inventory
    = Inventory Wallet Multipliers.Model Resources


clickAmount : Inventory -> Currency.Currency
clickAmount (Inventory _ multipliers _) =
    Multipliers.clickAmount multipliers


clickMultiplierCost : Inventory -> Currency.Currency
clickMultiplierCost (Inventory _ multipliers _) =
    Multipliers.clickMultiplierCost multipliers


setAvailableFunds : Currency.Currency -> Inventory -> Inventory
setAvailableFunds newFunds (Inventory _ multipliers resources) =
    Inventory (Wallet.fromCurrency newFunds) multipliers resources


initial : Inventory
initial =
    Inventory Wallet.initial Multipliers.initial initialResources


initialWithResources : List ( Resource.Level, Resource.Resource ) -> Inventory
initialWithResources =
    Inventory Wallet.initial Multipliers.initial << AllDict.fromList toString


generateCurrency : Inventory -> Inventory
generateCurrency ((Inventory wallet multipliers resources) as inventory) =
    let
        newWallet =
            Wallet.add (clickAmount inventory) wallet
    in
    Inventory newWallet multipliers resources


accrueValue : Float -> Inventory -> Inventory
accrueValue frequency (Inventory wallet multipliers resources) =
    let
        totalIncomeRate =
            Resource.totalIncomeRate <| AllDict.values resources

        accruedValueTotal =
            IncomeRate.toCurrency <| IncomeRate.multiply totalIncomeRate frequency

        newWallet =
            Wallet.add accruedValueTotal wallet
    in
    Inventory newWallet multipliers resources


availableFunds : Inventory -> Currency.Currency
availableFunds (Inventory wallet _ _) =
    Wallet.toCurrency wallet


resources : Inventory -> List Resource.Resource
resources (Inventory _ _ resources) =
    AllDict.values resources


resourcesWithLevels : Inventory -> List ( Resource.Level, Resource.Resource )
resourcesWithLevels (Inventory _ _ resources) =
    AllDict.toList resources


purchaseClickMultiplier : Inventory -> Inventory
purchaseClickMultiplier ((Inventory wallet multipliers resources) as inventory) =
    let
        finalCost =
            clickMultiplierCost inventory
    in
    if wallet |> canPayFor finalCost then
        Inventory (Wallet.subtract finalCost wallet) (Multipliers.incrementClickMultiplier multipliers) resources
    else
        Inventory wallet multipliers resources


purchaseResource : Int -> Resource.Level -> Inventory -> Inventory
purchaseResource count level (Inventory wallet multipliers resources) =
    case AllDict.get level resources of
        Nothing ->
            Inventory wallet multipliers resources

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
                Inventory (Wallet.subtract finalCost wallet) multipliers newResources
            else
                Inventory wallet multipliers resources


canPayFor : Currency.Currency -> Wallet -> Bool
canPayFor cost wallet =
    Currency.gte (Wallet.toCurrency wallet) cost


initialResources : Resources
initialResources =
    AllDict.fromList toString
        [ ( Resource.L1, Resource.build "Cursor" 0.1 1.07 (Currency.Currency 15) )
        , ( Resource.L2, Resource.build "Backpack" 1 1.07 (Currency.Currency 100) )
        , ( Resource.L3, Resource.build "Skateboard" 8 1.07 (Currency.Currency 1100) )
        , ( Resource.L4, Resource.build "Bicycle" 47 1.07 (Currency.Currency 12000) )
        ]
