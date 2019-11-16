import { TransactionType, Transaction } from "../typings/global"

const ignorableTransactions = (transactionType: TransactionType) => (
  transactionType != TransactionType.SALDO_CORRENTE && 
  transactionType != TransactionType.APLICACAO && 
  transactionType != TransactionType.VALOR_APLICACAO
)

const isRevenue = ({ value, transactionTypes }: Transaction) => (
  value <= 0 && transactionTypes.filter(ignorableTransactions)
) 

const isExpense = ({ value, transactionTypes }: Transaction) => {
  value > 0 && transactionTypes.filter(ignorableTransactions)
}

const getTransactionValues = (transactions: Transaction[]) => (
  transactions.map(({ value }: Transaction) => value)
)

const getRemains = (revenueValue: number, expenseValue: number) => (
  revenueValue - expenseValue
)
const getTotal = (revenueValue: number, expenseValue: number) => (
  revenueValue + expenseValue
)


export {
  isRevenue,
  isExpense,
  getTransactionValues,
  getRemains,
  getTotal
  
}