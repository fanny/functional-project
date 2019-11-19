module Helpers ( 
  isRevenue,
  isExpense,
  isRevenueOrExpense,
  checkYear,
  checkMonth
) where

import GregorianCalendar
import Transaction
import TransactionType
import Data.List

checkYear :: Integer -> Transaction -> Bool
checkYear y t = year (date t) == y

checkMonth :: Integer -> Transaction -> Bool
checkMonth m t = month (date t) == m

isRevenueOrExpense :: Transaction -> Bool
isRevenueOrExpense t = ([VALOR_APLICACAO, APLICACAO] `intersect` (transactionTypes t)) == []

isRevenue :: Transaction -> Bool
isRevenue t = (value t) >= 0 && (isRevenueOrExpense t)

isExpense :: Transaction -> Bool
isExpense t = (value t) < 0 && (isRevenueOrExpense t)