/*import { groupBy } from './util'
import { Transaction } from './typings/global';*/
import { getTotalBalanceByPeriod, getMinBalanceByPeriod, getMaxBalanceByPeriod } from './resolvers/queries'

/*
const groupMonths = (acc:any, { value }:Transaction) => acc.concat(value)
const getMonth = ({ date: { month }}: Transaction) => month

const a = groupBy(groupMonths, getMonth, filterByPeriod({year: 2017}))*/
console.log(getTotalBalanceByPeriod({year: 2017}))
console.log(getMinBalanceByPeriod({year: 2017}))
console.log(getMaxBalanceByPeriod({year: 2017}))

