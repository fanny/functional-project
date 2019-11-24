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

const getInitialBalance = (period: GregorianCalendar) => {
  const { value } = head(filterByPeriod(period))
  return value
}

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {  
  return getInitialBalance(period) + getRemainByPeriod(period) 
}

const getTransactionsByDay = (period: GregorianCalendar) => (
  groupBy(
    concatValues,
    getDay, 
    filterByRevenueOrExpense(filterByPeriod(period))
  )
)

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  return max(reduceGroups(getTransactionsByDay(period)).map(cumulativeSum(getInitialBalance(period))))
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  return min(reduceGroups(getTransactionsByDay(period)).map(cumulativeSum(getInitialBalance(period))))
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
  const transactionsByDay = getTransactionsByDay(period)
  const balances = reduceGroups(transactionsByDay).map(cumulativeSum(getInitialBalance(period)))
  const days = Object.keys(transactionsByDay)

  return days.map((day, i) => ({[day]: balances[i]}))
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