import { groupBy } from './util'
import { Transaction } from './typings/global';
import { filterByPeriod } from './resolvers/queries'


const groupMonths = (acc:any, { value }:Transaction) => acc.concat(value)
const toMonth = ({ date: { month }}: Transaction) => month

groupBy(groupMonths, toMonth, filterByPeriod({year: 2017}))


