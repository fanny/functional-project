/**
 * This code has a main purpose to export all filter functions.
*/

import {isRevenue, isExpense} from './helpers'
import * as transactions from '../../../data/transactions.json'
import {GregorianCalendar, Transaction} from '../typings/global'

const filterByPeriod = ({year, month}: GregorianCalendar) => {
  const pass = true
  const filteredTransactions = transactions.filter(
    ({date: dateTransaction}: Transaction) => (
      (month ? dateTransaction.month == month: pass) && 
      (year ? dateTransaction.year == year: pass)
    )
  )
  return filteredTransactions
}

const filterByRevenue = (transactions: Transaction[]) => (
  transactions.filter(isRevenue)
)

const filterByExpense = (transactions: Transaction[]) => (
  transactions.filter(isExpense)
)
const filterByRevenueOrExpense = (transactions: Transaction[]) => (
  transactions.filter(isRevenue).concat(transactions.filter(isExpense))
)

export {
  filterByPeriod,
  filterByRevenue,
  filterByExpense,
  filterByRevenueOrExpense
}
