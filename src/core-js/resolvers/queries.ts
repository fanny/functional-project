/**
 * This code has a main purpose to serve like an API, making available all methods allowed.
*/

import { filterByPeriod, filterByExpense, filterByRevenue } from './filters'
import { getTransactionsValues, getRemains, getTotal } from './helpers'
import { sum, zipWith, head, last, median } from '../util'
import { GregorianCalendar } from '../typings/global'

const getRevenueByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByRevenue(filteredTransactions)
}

const getExpensesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByExpense(filteredTransactions)
}

const getRemainsByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenueByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return zipWith(getRemains, revenues, expenses)
}

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenueByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return sum(zipWith(getTotal, revenues, expenses))
}

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenueByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return last(sortedBalance)
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenueByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return head(sortedBalance)
}

const getAvgRevenuesByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenueByPeriod(period))

  return median(revenues)
}

const getAvgExpensesByPeriod = (period: GregorianCalendar) => {
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return median(expenses)
}

const getAvgRemainsByPeriod = (period: GregorianCalendar) => {
  const remains = getRemainsByPeriod(period)

  return median(remains)
}

export {
  getRevenueByPeriod,
  getExpensesByPeriod,
  getRemainsByPeriod,
  getTotalBalanceByPeriod,
  getMaxBalanceByPeriod,
  getMinBalanceByPeriod,
  getAvgRevenuesByPeriod,
  getAvgExpensesByPeriod,
  getAvgRemainsByPeriod,
  filterByPeriod
}