/**
 * This code has a main purpose to serve like an API, making available all methods allowed.
*/

import {
  filterByPeriod,
  filterByExpense,
  filterByRevenue,
  filterByRevenueOrExpense
} from './filters'
import {
  getTransactionsValues,
  concatValues,
  getDay,
  getMonth
} from './helpers'
import {
  sum,
  head,
  average,
  groupBy,
  cumulativeSum,
  min,
  max,
  reduceGroups
} from '../util'
import { GregorianCalendar } from '../typings/global'

const getRevenuesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByRevenue(filteredTransactions)
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

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {
  const { value } = head(filterByPeriod(period))
  
  return value + getRemainByPeriod(period) 
}

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  const { value } = head(filterByPeriod(period))
  const filteredTransactions = groupBy(
    concatValues,
    getDay, 
    filterByRevenueOrExpense(filterByPeriod(period))
  )

  return max(reduceGroups(filteredTransactions).map(cumulativeSum(value)))
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const { value } = head(filterByPeriod(period))
  const filteredTransactions = groupBy(
    concatValues,
    getDay,
    filterByRevenueOrExpense(filterByPeriod(period))
  )

  return min(reduceGroups(filteredTransactions).map(cumulativeSum(value)))
}

const getAvgRevenuesByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionsValues(getRevenuesByPeriod(period))

  return average(revenues)
}

const getAvgExpensesByPeriod = (period: GregorianCalendar) => {
  const expenses = getTransactionsValues(getExpensesByPeriod(period))

  return average(expenses) * (-1)
}

const getAvgRemainsByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = groupBy(
    concatValues,
    getMonth, 
    filterByRevenueOrExpense(filterByPeriod(period))
  )

  return average(reduceGroups(filteredTransactions))
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