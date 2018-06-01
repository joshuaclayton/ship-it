module Data.Inventory
    exposing
        ( Inventory
        , accrueValue
        , addLimitedMultiplier
        , availableFunds
        , canSpend
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
        , tickMultipliers
        )

import AllDict
import Data.Currency as Currency
import Data.GameConfiguration as Config
import Data.IncomeRate as IncomeRate
import Data.Increasable as Increasable
import Data.Multipliers as Multipliers
import Data.Multipliers.Limited as LimitedMultipliers
import Data.Resource as Resource
import Data.Wallet as Wallet exposing (Wallet)


type alias Resources =
    Config.LevelDict Resource.Resource


type Inventory
    = Inventory
        { wallet : Wallet
        , multipliers : Multipliers.Model
        , discounts : LimitedMultipliers.Model
        , resources : Resources
        }


clickAmount : Inventory -> Currency.Currency
clickAmount (Inventory { multipliers }) =
    Multipliers.clickAmount multipliers


clickMultiplierCost : Inventory -> Currency.Currency
clickMultiplierCost (Inventory { multipliers }) =
    Multipliers.clickMultiplierCost multipliers


resourceMultiplierCost : Inventory -> Config.Level -> Currency.Currency
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
        , discounts = LimitedMultipliers.initial
        , resources = initialResources
        }


tickMultipliers : Inventory -> Inventory
tickMultipliers (Inventory ({ multipliers } as inventory)) =
    Inventory { inventory | multipliers = Multipliers.tick multipliers }


initialWithResources : List ( Config.Level, Resource.Resource ) -> Inventory
initialWithResources resources =
    Inventory
        { wallet = Wallet.initial
        , multipliers = Multipliers.initial
        , discounts = LimitedMultipliers.initial
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


resourcesWithLevels : Inventory -> List ( Config.Level, Resource.Resource )
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


purchaseResourceMultiplier : Inventory -> Config.Level -> Inventory
purchaseResourceMultiplier ((Inventory ({ wallet, multipliers } as inventory)) as inv) level =
    let
        finalCost =
            resourceMultiplierCost inv level
    in
    if wallet |> canPayFor finalCost then
        Inventory
            { inventory
                | wallet = Wallet.subtract finalCost wallet
                , multipliers = Multipliers.incrementResourceMultiplier level multipliers
            }
    else
        inv


purchaseResource : Int -> Config.Level -> Inventory -> Inventory
purchaseResource count level (Inventory ({ resources, discounts, wallet } as inventory)) =
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

                preDiscountCost =
                    Resource.transactionCost transaction

                finalCost =
                    applyPurchaseDiscounts level discounts preDiscountCost
            in
            if wallet |> canPayFor finalCost then
                Inventory
                    { inventory
                        | wallet = Wallet.subtract finalCost wallet
                        , resources = newResources
                    }
            else
                Inventory inventory


applyPurchaseDiscounts : Config.Level -> LimitedMultipliers.Model -> Currency.Currency -> Currency.Currency
applyPurchaseDiscounts level discounts preDiscountCost =
    level
        |> LimitedMultipliers.resourceLevelMultipliers discounts
        |> Increasable.multiplierValue
        |> Currency.multiply preDiscountCost


addLimitedMultiplier : LimitedMultipliers.MultiplierType -> Inventory -> Inventory
addLimitedMultiplier multiplierType (Inventory ({ multipliers } as inventory)) =
    Inventory { inventory | multipliers = Multipliers.addLimitedMultiplier multiplierType multipliers }


canSpend : Currency.Currency -> Inventory -> Bool
canSpend cost (Inventory { wallet }) =
    canPayFor cost wallet


canPayFor : Currency.Currency -> Wallet -> Bool
canPayFor cost wallet =
    Currency.gte (Wallet.toCurrency wallet) cost


initialResources : Resources
initialResources =
    Config.buildLevelDict
        (\level ->
            Resource.build
                (Config.levelName level)
                (Config.levelIncomeRate level)
                (Config.increasableMultiplier level)
                (Config.levelBaseCost level)
        )
