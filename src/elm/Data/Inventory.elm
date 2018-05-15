module Data.Inventory
    exposing
        ( Inventory
        , accrueValue
        , availableFunds
        , generateCurrency
        , initial
        , purchaseResource
        , replaceResources
        , resources
        , setAvailableFunds
        )

import Data.Currency as Currency
import Data.IncomeRate as IncomeRate
import Data.Resource as Resource


type Wallet
    = Wallet Currency.Currency


type Inventory
    = Inventory Wallet (List Resource.Resource)


replaceResources : List Resource.Resource -> Inventory -> Inventory
replaceResources newResources (Inventory wallet _) =
    Inventory wallet newResources


setAvailableFunds : Currency.Currency -> Inventory -> Inventory
setAvailableFunds newFunds (Inventory _ resources) =
    Inventory (Wallet newFunds) resources


emptyWallet : Wallet
emptyWallet =
    Wallet Currency.zero


initial : Inventory
initial =
    Inventory emptyWallet initialResources


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
            Resource.totalIncomeRate resources

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
resources (Inventory _ list) =
    list


purchaseResource : Int -> Resource.Resource -> Inventory -> Inventory
purchaseResource count resource (Inventory wallet resources) =
    let
        transaction =
            Resource.purchase count resource

        newResource =
            Resource.applyTransaction transaction

        newResources =
            List.map (Resource.replace resource newResource) resources

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


initialResources : List Resource.Resource
initialResources =
    [ Resource.build "Cursor" 0.1 1.07 (Currency.Currency 15)
    , Resource.build "Backpack" 1 1.07 (Currency.Currency 100)
    , Resource.build "Skateboard" 8 1.07 (Currency.Currency 1100)
    , Resource.build "Bicycle" 47 1.07 (Currency.Currency 12000)
    ]


deductFromWallet : Currency.Currency -> Wallet -> Wallet
deductFromWallet amount (Wallet funds) =
    Wallet <| Currency.subtract amount funds
