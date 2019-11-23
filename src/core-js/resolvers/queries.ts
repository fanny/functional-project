/**
 * This code has a main purpose to serve like an API, making available all methods allowed.
*/

import { filterByPeriod, filterByExpense, filterByRevenue } from './filters'
import { getTransactionsValues, getRemains, getTotal, getMonth, concatValues, getDay } from './helpers'
import { sum, zipWith, head, last, average, groupBy } from '../util'
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
  const revenues = groupBy(concatValues, getMonth, getRevenuesByPeriod(period))
  const expenses = groupBy(concatValues, getMonth, getExpensesByPeriod(period))

  return zipWith(getRemains, revenues, expenses)
}

const getRemainByPeriod = (period: GregorianCalendar) => {
  return sum(getRemainsByPeriod(period))
}

const getTotalBalancesByPeriod = (period: GregorianCalendar) => {
  const revenues = groupBy(concatValues, getDay, getRevenuesByPeriod(period))
  const expenses = groupBy(concatValues, getDay, getRevenuesByPeriod(period))

  return zipWith(getTotal, revenues, expenses)
}

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {
  return sum(getTotalBalancesByPeriod(period))
}

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  const sortedBalance = getTotalBalancesByPeriod(period).sort()

  return last(sortedBalance)
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const sortedBalance = getTotalBalancesByPeriod(period).sort()

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

const getCashFlow = (period: GregorianCalendar) => {
  const transactionsByDay = groupBy(concatValues, getDay, getRevenuesByPeriod(period))
  const days = Object.keys(transactionsByDay)

  return days.map(day => [day, sum(transactionsByDay[day])])
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
  getCashFlow,
  filterByPeriod
}