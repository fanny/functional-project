import teste from './resolvers/queries'

const filterByPeriod = () => {
  console.log(teste.filterByPeriod({year: 2017}).slice(10))
  return teste.filterByPeriod({year: 2017}).slice(10)
}

export default filterByPeriod