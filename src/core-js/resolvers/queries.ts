/**
 * This code has a main purpose to serve like an API, making available all methods allowed.
*/

import { filterByPeriod, filterByExpense, filterByRevenue } from './filters'
import { getTransactionsValues, getTotal, concatValues, getDay } from './helpers'
import { sum, zipWith, head, last, average, groupBy } from '../util'
import { GregorianCalendar } from '../typings/global'

const getRevenuesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByRevenue((filteredTransactions))
}

const getRevenueByPeriod = (period: GregorianCalendar) => {
  return sum(getTransactionsValues(getRevenuesByPeriod(period)))
}

const getExpensesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByExpense(filteredTransactions)
}

const getExpenseByPeriod = (period: GregorianCalendar) => {
  return sum(getTransactionsValues(getExpensesByPeriod(period))) *(-1)
}

const getRemainByPeriod = (period: GregorianCalendar) => {
  const revenueValue = getRevenueByPeriod(period)
  const expenseValue = getExpenseByPeriod(period)

  return revenueValue - expenseValue
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
  const remains = getRemainByPeriod(period)

  return remains
}

const getCashFlow = (period: GregorianCalendar) => {
  const transactionsByDay = groupBy(concatValues, getDay, getRevenuesByPeriod(period))
  const days = Object.keys(transactionsByDay)

  return days.map(day => [day, sum(transactionsByDay[day])])
}

export {
  getRevenueByPeriod,
  getExpenseByPeriod,
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