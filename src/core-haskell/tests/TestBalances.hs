module TestBalances (
  testBalances
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo do saldo final em um determinado mês e ano.
testFinalBalance = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo final em Janeiro de 2018" 41812.6 (getFinalBalance db 2018 0)
      assertEqual "Calcula o saldo final em Maio de 2018" 57822.57 (getFinalBalance db 2018 4)
      assertEqual "Calcula o saldo final em Janeiro de 2019" 135209.42 (getFinalBalance db 2019 0)
      assertEqual "Calcula o saldo final em Abril de 2019" 154276.18000000002 (getFinalBalance db 2019 3)
    )

-- Testes referentes ao cálculo do saldo máximo em um determinado mês e ano.
testMaxBalance = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo máximo em Janeiro de 2018" 41812.6 (getMaxBalance db 2018 0)
      assertEqual "Calcula o saldo máximo em Maio de 2018" 57822.57 (getMaxBalance db 2018 4)
      assertEqual "Calcula o saldo máximo em Janeiro de 2019" 135209.42 (getMaxBalance db 2019 0)
      assertEqual "Calcula o saldo máximo em Abril de 2019" 154276.18000000002 (getMaxBalance db 2019 3)
    )

-- Testes referentes ao cálculo do saldo mínimo em um determinado mês e ano.
testMinBalance = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo mínimo em Janeiro de 2018" 41812.6 (getMinBalance db 2018 0)
      assertEqual "Calcula o saldo mínimo em Maio de 2018" 57822.57 (getMinBalance db 2018 4)
      assertEqual "Calcula o saldo mínimo em Janeiro de 2019" 130801.66 (getMinBalance db 2019 0)
      assertEqual "Calcula o saldo mínimo em Abril de 2019" 153797.14 (getMinBalance db 2019 3)
    )

testBalances = 
  TestList [
    testFinalBalance,
    testMaxBalance,
    testMinBalance
  ]