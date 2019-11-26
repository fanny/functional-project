module TestValues (
  testValues
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo do valor da receita em um determinado mês e ano.
testRevenueValue01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da receita em Janeiro de 2018" 92.45 (getRevenueValue db 2018 0)
    )

testRevenueValue02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da receita em Maio de 2018" 929.44 (getRevenueValue db 2018 4)
  )

testRevenueValue03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da receita em Janeiro de 2019" 4407.76 (getRevenueValue db 2019 0)
  )

testRevenueValue04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da receita em Abril de 2019" 479.04 (getRevenueValue db 2019 3)
  )

-- Testes referentes ao cálculo do valor da despesa em um determinado mês e ano.
testExpenseValue01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da despesas em Janeiro de 2018" 2260.0 (getExpenseValue db 2018 0)
    )

testExpenseValue02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da despesas em Maio de 2018" 250.0 (getExpenseValue db 2018 4)
  )

testExpenseValue03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da despesas em Janeiro de 2019" 500.0 (getExpenseValue db 2019 0)
  )

testExpenseValue04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da despesas em Abril de 2019" 10000.0 (getExpenseValue db 2019 3)
  )

-- Testes referentes ao cálculo do valor da sobra em um determinado mês e ano.
testRemainsValue01 = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da sobra em Janeiro de 2018" (-2167.55) (getRemainsValue db 2018 0)
    )

testRemainsValue02 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da sobra em Maio de 2018" 679.44 (getRemainsValue db 2018 4)
  )

testRemainsValue03 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da sobra em Janeiro de 2019" 3907.76 (getRemainsValue db 2019 0)
  )

testRemainsValue04 = TestCase (
  do
    db <- getTestTransactions
    assertEqual "Calcula o valor da sobra em Abril de 2019" (-9520.96) (getRemainsValue db 2019 3)
  )

testValues = 
  TestList [
    testExpenseValue01,
    testExpenseValue02,
    testExpenseValue03,
    testExpenseValue04,
    testRevenueValue01,
    testRevenueValue02,
    testRevenueValue03,
    testRevenueValue04,
    testRemainsValue01,
    testRemainsValue02,
    testRemainsValue03,
    testRemainsValue04
  ]