module TestValues (
  testValues
) where

import Test.HUnit
import JsonParser
import Queries

-- Testes referentes ao cálculo do valor da receita em um determinado mês e ano.
testRevenueValue = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da receita em Janeiro de 2018" 92.45 (getRevenueValue db 2018 0)
      assertEqual "Calcula o valor da receita em Maio de 2018" 929.44 (getRevenueValue db 2018 4)
      assertEqual "Calcula o valor da receita em Janeiro de 2019" 4407.76 (getRevenueValue db 2019 0)
      assertEqual "Calcula o valor da receita em Abril de 2019" 479.04 (getRevenueValue db 2019 3)
    )

-- Testes referentes ao cálculo do valor da despesa em um determinado mês e ano.
testExpenseValue = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da despesas em Janeiro de 2018" 2260.0 (getExpenseValue db 2018 0)
      assertEqual "Calcula o valor da despesas em Maio de 2018" 250.0 (getExpenseValue db 2018 4)
      assertEqual "Calcula o valor da despesas em Janeiro de 2019" 500.0 (getExpenseValue db 2019 0)
      assertEqual "Calcula o valor da despesas em Abril de 2019" 10000.0 (getExpenseValue db 2019 3)
    )

-- Testes referentes ao cálculo do valor da sobra em um determinado mês e ano.
testRemainsValue = TestCase (
    do
      db <- getTestTransactions
      assertEqual "Calcula o valor da sobra em Janeiro de 2018" (-2167.55) (getRemainsValue db 2018 0)
      assertEqual "Calcula o valor da sobra em Maio de 2018" 679.44 (getRemainsValue db 2018 4)
      assertEqual "Calcula o valor da sobra em Janeiro de 2019" 3907.76 (getRemainsValue db 2019 0)
      assertEqual "Calcula o valor da sobra em Abril de 2019" (-9520.96) (getRemainsValue db 2019 3)
    )

testValues = 
  TestList [
    testExpenseValue,
    testRevenueValue,
    testRemainsValue
  ]