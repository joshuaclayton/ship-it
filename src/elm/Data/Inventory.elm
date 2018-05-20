module Data.Inventory
    exposing
        ( Inventory
        , accrueValue
        , availableFunds
        , clickAmount
        , clickMultiplierCost
        , currentIncomeRate
        , generateCurrency
        , initial
        , initialWithResources
        , purchaseClickMultiplier
        , purchaseResource
        , purchaseResourceMultiplier
        , resourceMultiplierCost
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
    = Inventory
        { wallet : Wallet
        , multipliers : Multipliers.Model
        , resources : Resources
        }


clickAmount : Inventory -> Currency.Currency
clickAmount (Inventory { multipliers }) =
    Multipliers.clickAmount multipliers


clickMultiplierCost : Inventory -> Currency.Currency
clickMultiplierCost (Inventory { multipliers }) =
    Multipliers.clickMultiplierCost multipliers


resourceMultiplierCost : Inventory -> Resource.Level -> Currency.Currency
resourceMultiplierCost (Inventory { multipliers }) =
    Multipliers.resourceMultiplierCost multipliers


setAvailableFunds : Currency.Currency -> Inventory -> Inventory
setAvailableFunds newFunds (Inventory inventory) =
    Inventory { inventory | wallet = Wallet.fromCurrency newFunds }


initial : Inventory
initial =
    Inventory
        { wallet = Wallet.initial
        , multipliers = Multipliers.initial
        , resources = initialResources
        }


initialWithResources : List ( Resource.Level, Resource.Resource ) -> Inventory
initialWithResources resources =
    Inventory
        { wallet = Wallet.initial
        , multipliers = Multipliers.initial
        , resources = AllDict.fromList toString resources
        }


generateCurrency : Inventory -> Inventory
generateCurrency ((Inventory ({ wallet } as inventory)) as inv) =
    let
        newWallet =
            Wallet.add (clickAmount inv) wallet
    in
    Inventory { inventory | wallet = newWallet }


currentIncomeRate : Inventory -> IncomeRate.IncomeRate
currentIncomeRate (Inventory { resources, multipliers }) =
    let
        applyIncomeRateMultipliers level resource =
            Multipliers.increaseMultiplierForLevel multipliers level
                |> Resource.withIncomeRateMultiplier resource
    in
    AllDict.map applyIncomeRateMultipliers resources
        |> AllDict.values
        |> Resource.totalIncomeRate


accrueValue : Float -> Inventory -> Inventory
accrueValue frequency ((Inventory ({ wallet } as inventory)) as inv) =
    let
        accruedValueTotal =
            IncomeRate.toCurrency <| IncomeRate.multiply (currentIncomeRate inv) frequency

        newWallet =
            Wallet.add accruedValueTotal wallet
    in
    Inventory { inventory | wallet = newWallet }


availableFunds : Inventory -> Currency.Currency
availableFunds (Inventory { wallet }) =
    Wallet.toCurrency wallet


resources : Inventory -> List Resource.Resource
resources (Inventory { resources }) =
    AllDict.values resources


resourcesWithLevels : Inventory -> List ( Resource.Level, Resource.Resource )
resourcesWithLevels (Inventory { resources }) =
    AllDict.toList resources


purchaseClickMultiplier : Inventory -> Inventory
purchaseClickMultiplier ((Inventory ({ wallet, multipliers } as inventory)) as inv) =
    let
        finalCost =
            clickMultiplierCost inv
    in
    if wallet |> canPayFor finalCost then
        Inventory
            { inventory
                | wallet = Wallet.subtract finalCost wallet
                , multipliers = Multipliers.incrementClickMultiplier multipliers
            }
    else
        inv


purchaseResourceMultiplier : Inventory -> Resource.Level -> Inventory
purchaseResourceMultiplier ((Inventory ({ wallet, multipliers } as inventory)) as inv) level =
    let
        finalCost =
            resourceMultiplierCost inv level
    in
    if wallet |> canPayFor finalCost then
        Inventory
            { inventory
                | wallet = Wallet.subtract finalCost wallet
                , multipliers = Multipliers.incrementResourceMultiplier multipliers level
            }
    else
        inv


purchaseResource : Int -> Resource.Level -> Inventory -> Inventory
purchaseResource count level (Inventory ({ resources, wallet } as inventory)) =
    case AllDict.get level resources of
        Nothing ->
            Inventory inventory

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
                Inventory
                    { inventory
                        | wallet = Wallet.subtract finalCost wallet
                        , resources = newResources
                    }
            else
                Inventory inventory


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
