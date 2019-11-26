module TestBalances (
  testBalances
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo do saldo final em um determinado mês e ano.
testFinalBalance01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo final em Janeiro de 2018" 41812.6 (getFinalBalance db 2018 0)
    )

testFinalBalance02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo final em Maio de 2018" 57822.57 (getFinalBalance db 2018 4)
  )

testFinalBalance03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo final em Janeiro de 2019" 135209.42 (getFinalBalance db 2019 0)
  )

testFinalBalance04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo final em Abril de 2019" 154276.18000000002 (getFinalBalance db 2019 3)
  )

-- Testes referentes ao cálculo do saldo máximo em um determinado mês e ano.
testMaxBalance01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo máximo em Janeiro de 2018" 41812.6 (getMaxBalance db 2018 0)
    )

testMaxBalance02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo máximo em Maio de 2018" 57822.57 (getMaxBalance db 2018 4)
  )

testMaxBalance03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo máximo em Janeiro de 2019" 135209.42 (getMaxBalance db 2019 0)
  )

testMaxBalance04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo máximo em Abril de 2019" 154276.18000000002 (getMaxBalance db 2019 3)
  )

-- Testes referentes ao cálculo do saldo mínimo em um determinado mês e ano.
testMinBalance01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o saldo mínimo em Janeiro de 2018" 41812.6 (getMinBalance db 2018 0)
    )

testMinBalance02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo mínimo em Maio de 2018" 57822.57 (getMinBalance db 2018 4)
  )

testMinBalance03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo mínimo em Janeiro de 2019" 130801.66 (getMinBalance db 2019 0)
  )

testMinBalance04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o saldo mínimo em Abril de 2019" 153797.14 (getMinBalance db 2019 3)
  )

testBalances = 
  TestList [
    testFinalBalance01,
    testFinalBalance02,
    testFinalBalance03,
    testFinalBalance04,
    testMaxBalance01,
    testMaxBalance02,
    testMaxBalance03,
    testMaxBalance04,
    testMinBalance01,
    testMinBalance02,
    testMinBalance03,
    testMinBalance04
  ]