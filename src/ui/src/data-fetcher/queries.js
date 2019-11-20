import {filterByPeriod as filterByPeriodBase} from '../../../core-js/resolvers/queries.ts'

const filterByPeriod = () => {
  console.log(filterByPeriodBase({year: 2017}).slice(0, 10))
  return filterByPeriodBase({year: 2017}).slice(0, 10)
}

export default filterByPeriod