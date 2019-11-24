import {
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
} from './resolvers/queries'


console.log(getRevenueByPeriod({year: 2019, month: 4}))
console.log(getExpenseByPeriod({year: 2019, month: 4}))
console.log(getRemainByPeriod({year: 2019, month: 4}))
console.log(getTotalBalanceByPeriod({year: 2019, month: 4}))
console.log(getMinBalanceByPeriod({year: 2019, month: 4}))
console.log(getAvgRevenuesByPeriod({year: 2019}))
console.log(getAvgExpensesByPeriod({year: 2019}))
console.log(getAvgRemainsByPeriod({year: 2019}))
console.log(getCashFlow({year: 2019, month: 4}))
console.log(getMaxBalanceByPeriod({year: 2019, month: 4}))


