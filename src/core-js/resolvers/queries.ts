import { GregorianCalendar, Transaction } from "../typings/global"
import * as transactions from '../../../data/transactions.json'
import { isRevenue, isExpense, getTransactionValues, getRemains, getTotal } from './helpers'
import { sum, zipWith, head, last, median } from '../util'


const getRevenueByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)
  return filterByRevenue(filteredTransactions)

}

const getExpensesByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = filterByPeriod(period)
  return filterByExpense(filteredTransactions)
}

const getRemainsByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionValues(getRevenueByPeriod(period))
  const expenses = getTransactionValues(getExpensesByPeriod(period))

  return zipWith(getRemains, revenues, expenses)
}

const getTotalBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionValues(getRevenueByPeriod(period))
  const expenses = getTransactionValues(getExpensesByPeriod(period))

  return sum(zipWith(getTotal, revenues, expenses))
}

const getMaxBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionValues(getRevenueByPeriod(period))
  const expenses = getTransactionValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return last(sortedBalance)
}

const getMinBalanceByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionValues(getRevenueByPeriod(period))
  const expenses = getTransactionValues(getExpensesByPeriod(period))
  const sortedBalance = zipWith(getTotal, revenues, expenses).sort()

  return head(sortedBalance)
}

const getAvgRevenueByPeriod = (period: GregorianCalendar) => {
  const revenues = getTransactionValues(getRevenueByPeriod(period))
  return median(revenues)
}

const getAvgExpensesByPeriod = (period: GregorianCalendar) => {
  const expenses = getTransactionValues(getExpensesByPeriod(period))
  return median(expenses)
}

const getAvgRemainsByPeriod = (period: GregorianCalendar) => {
  const remains = getRemainsByPeriod(period)
  return median(remains)
}

const filterByPeriod = (period: GregorianCalendar) => {
  const filteredTransactions = transactions.filter((transaction: Transaction) => {
    return transaction.date === period
  })

  return filteredTransactions
}

const filterByRevenue = (transactions: Transaction[]) => (
  transactions.filter(isRevenue)
)
const filterByExpense = (transactions: Transaction[]) => (
  transactions.filter(isExpense)
)

export {
  getRevenueByPeriod,
  getExpensesByPeriod,
  getRemainsByPeriod,
  getTotalBalanceByPeriod,
  getMaxBalanceByPeriod,
  getMinBalanceByPeriod,
  getAvgRevenueByPeriod,
  getAvgExpensesByPeriod,
  getAvgRemainsByPeriod,
  filterByPeriod
}