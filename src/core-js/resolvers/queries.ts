/**
 * This code has a main purpose to serve like an API, making available all methods allowed.
*/

import { filterByPeriod, filterByExpense, filterByRevenue } from './filters'
import { getTransactionsValues, getRemains, getTotal, getMonth, groupMonths} from './helpers'
import { sum, zipWith, head, last, average, groupBy, flatten } from '../util'
import { GregorianCalendar } from '../typings/global'

const getRevenuesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByRevenue(filteredTransactions)
}

const getExpensesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByExpense(filteredTransactions)
}

const getRemainsByPeriod = (period: GregorianCalendar) => {
  const revenuesByMonth = groupBy(groupMonths, getMonth, getRevenuesByPeriod(period))
  const expensesByMonth = groupBy(groupMonths, getMonth, getExpensesByPeriod(period))

  const revenues = flatten(Object.values(revenuesByMonth))
  const expenses = flatten(Object.values(expensesByMonth))

  return zipWith(getRemains, revenues, expenses)
}

const getRemainByPeriod = (period: GregorianCalendar) => {
  return sum(getRemainsByPeriod(period))
}

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenuesByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return sum(zipWith(getTotal, revenues, expenses))
}

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenuesByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return last(sortedBalance)
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenuesByPeriod(period))
  const expenses = getTransactionsValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return head(sortedBalance)
}

const getAvgRevenuesByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenuesByPeriod(period))

  return average(revenues)
}

const getAvgExpensesByPeriod = (period: GregorianCalendar) => {
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return average(expenses)
}

const getAvgRemainsByPeriod = (period: GregorianCalendar) => {
  const remains = getRemainsByPeriod(period)

  return average(remains)
}

export {
  getRevenuesByPeriod,
  getExpensesByPeriod,
  getRemainByPeriod,
  getTotalBalanceByPeriod,
  getMaxBalanceByPeriod,
  getMinBalanceByPeriod,
  getAvgRevenuesByPeriod,
  getAvgExpensesByPeriod,
  getAvgRemainsByPeriod,
  filterByPeriod
}