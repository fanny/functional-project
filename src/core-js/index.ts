/*import { groupBy } from './util'
import { Transaction } from './typings/global';*/
import {
  getMaxBalanceByPeriod
} from './resolvers/queries'


console.log(getMaxBalanceByPeriod({year: 2019, month: 1}))


