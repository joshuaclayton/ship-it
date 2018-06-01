module Data.Wallet
    exposing
        ( Wallet
        , add
        , extractCurrency
        , fromCurrency
        , initial
        , subtract
        , toCurrency
        )

import Data.Currency as Currency


type Wallet
    = Wallet Currency.Currency


initial : Wallet
initial =
    Wallet Currency.zero


fromCurrency : Currency.Currency -> Wallet
fromCurrency =
    Wallet


toCurrency : Wallet -> Currency.Currency
toCurrency (Wallet funds) =
    funds


add : Currency.Currency -> Wallet -> Wallet
add currency =
    map (Currency.add currency)


subtract : Currency.Currency -> Wallet -> Wallet
subtract amount =
    map (Currency.subtract amount)


map : (Currency.Currency -> Currency.Currency) -> Wallet -> Wallet
map f (Wallet funds) =
    Wallet <| f funds


extractCurrency : Wallet -> Currency.Currency
extractCurrency (Wallet currency) =
    currency
