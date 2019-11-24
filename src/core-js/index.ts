/*import { groupBy } from './util'
import { Transaction } from './typings/global';*/
import {
  getCashFlow
} from './resolvers/queries'


console.log(getCashFlow({year: 2019, month: 1}))


