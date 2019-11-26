module TestFilters (
  testFilters
) where

import Test.HUnit
import Data.List
import JsonParser
import Queries
import Transaction

-- Testes referentes ao filtro de transações por ano
testFilterYear01 = TestCase (
    do
      db <- getTransactions
      assertEqual "Filtra transações pelo ano 2018 - Checa primeiro elemento" 43980.15 (value (head (getTransactionsByYear db 2018)))
    )

testFilterYear02 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2018 - Checa segundo elemento" 2602.34 (value ((getTransactionsByYear db 2018) !! 1))
  )

testFilterYear03 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2018 - Checa penúltimo elemento" (-4.0) (value (last (init (getTransactionsByYear db 2018))))
  )

testFilterYear04 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2018 - Checa último elemento" 3471.24 (value (last (getTransactionsByYear db 2018)))
  )

-- Testes referentes ao filtro de transações por ano e mês
testFilterYearAndMonth01 = TestCase (
    do
      db <- getTransactions
      assertEqual "Filtra transações pelo ano 2019 no mês 3 - Checa primeiro elemento" 163797.14 (value (head (getTransactionsByYearAndMonth db 2019 3)))
    )

testFilterYearAndMonth02 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2019 no mês 3 - Checa segundo elemento" (-10000.0) (value ((getTransactionsByYearAndMonth db 2019 3) !! 1))
  )

testFilterYearAndMonth03 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2019 no mês 3 - Checa penúltimo elemento" (-3.0) (value (last (init (getTransactionsByYearAndMonth db 2019 3))))
  )

testFilterYearAndMonth04 = TestCase (
  do
    db <- getTransactions
    assertEqual "Filtra transações pelo ano 2019 no mês 3 - Checa último elemento" 530.68 (value (last (getTransactionsByYearAndMonth db 2019 3)))
  )

testFilters = 
  TestList [
    testFilterYear01,
    testFilterYear02,
    testFilterYear03,
    testFilterYear04,
    testFilterYearAndMonth01,
    testFilterYearAndMonth02,
    testFilterYearAndMonth03,
    testFilterYearAndMonth04
  ]