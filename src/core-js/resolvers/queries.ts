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
import transactions from '../../../data/transactions.json'

const getRevenuesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByRevenue(filteredTransactions)
}

const getRevenuesValues = (period: GregorianCalendar) => {
  return getTransactionsValues(getRevenuesByPeriod(period))
}

const getRevenueByPeriod = (period: GregorianCalendar) => {
  return sum(getRevenuesValues(period))
}

const getExpensesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)

  return filterByExpense(filteredTransactions)
}

const getExpensesValues = (period: GregorianCalendar) => {
  return getTransactionsValues(getExpensesByPeriod(period))
}

const getExpenseByPeriod = (period: GregorianCalendar) => {
  return sum(getExpensesValues(period))*(-1)
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
  const dayBalances = reduceGroups(getTransactionsByDay(period))

  return max(dayBalances.map(cumulativeSum(getInitialBalance(period))))
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const dayBalances = reduceGroups(getTransactionsByDay(period))
  
  return min(dayBalances.map(cumulativeSum(getInitialBalance(period))))
}

const getAvgRevenuesByPeriod = (period: GregorianCalendar) => {
  const revenues = getRevenuesValues((period))

  return average(revenues)
}

const getAvgExpensesByPeriod = (period: GregorianCalendar) => {
  const expenses = getExpensesValues((period))

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

const getYears = () => (
  new Set(transactions.map(transaction => transaction.date.year))
)
const getMonths = () => (
  new Set(transactions.map(transaction => transaction.date.month))
)

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
  filterByPeriod,
  getYears,
  getMonths
}