/**
 * This code has a main purpose to compose other functions, for help them to complete the task. 
*/

import { TransactionType, Transaction } from '../typings/global'

const isTransaction = (transactionType: string) => (
  transactionType != TransactionType.SALDO_CORRENTE && 
  transactionType != TransactionType.APLICACAO && 
  transactionType != TransactionType.VALOR_APLICACAO
)

const isRevenue = ({ value, transactionTypes }: Transaction) => (
  value <= 0 && transactionTypes.filter(isTransaction)
) 

const isExpense = ({ value, transactionTypes }: Transaction) => (
  value > 0 && transactionTypes.filter(isTransaction)
)

const getTransactionsValues = (transactions: Transaction[]) => (
  transactions.map(({ value }: Transaction) => value)
)

const getRemains = (revenueValue: number, expenseValue: number) => (
  revenueValue - (expenseValue * (-1))
)

const getTotal = (revenueValue: number, expenseValue: number) => (
  revenueValue + expenseValue
)

const concatValues = (acc:any, { value }:Transaction) => acc.concat(value)
const getMonth = ({ date: { month }}: Transaction) => month
const getDay = ({ date: { dayOfMonth }}: Transaction) => dayOfMonth

export {
  isRevenue,
  isExpense,
  getTransactionsValues,
  getRemains,
  getTotal,
  concatValues,
  getMonth,
  getDay
}