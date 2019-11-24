/*import { groupBy } from './util'
import { Transaction } from './typings/global';*/
import {
  getRevenueByPeriod,
  getExpenseByPeriod,
 /* getRemainByPeriod,
  getTotalBalanceByPeriod,
  getMaxBalanceByPeriod,
  getMinBalanceByPeriod,
  getAvgRevenuesByPeriod,
  getAvgExpensesByPeriod,
  getAvgRemainsByPeriod,
  getCashFlow,*/
} from './resolvers/queries'

/*
const groupMonths = (acc:any, { value }:Transaction) => acc.concat(value)
const getMonth = ({ date: { month }}: Transaction) => month

const a = groupBy(groupMonths, getMonth, filterByPeriod({year: 2017}))*/
/*console.log(getTotalBalanceByPeriod({year: 2019, month: 1}))
console.log(getMinBalanceByPeriod({year: 2019, month: 1}))
console.log(getMaxBalanceByPeriod({year: 2019, month: 1}))*/
console.log(getRevenueByPeriod({year: 2019, month: 1}))
console.log(getExpenseByPeriod({year: 2019, month: 1}))
/*
console.log(getRemainByPeriod({year: 2019, month: 1}))
console.log(getAvgRevenuesByPeriod({year: 2019}))
console.log(getAvgExpensesByPeriod({year: 2019}))
console.log(getAvgRemainsByPeriod({year: 2019, month: 1}))
console.log(getCashFlow({year: 2019, month: 1}))*/


