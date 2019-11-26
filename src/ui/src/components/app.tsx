import React from 'react'
import Card from  './SummarizingCard/index'
import Select from  './Select/index'
import {
    getRevenueByPeriod,
    getExpenseByPeriod,
    getRemainByPeriod,
    getTotalBalanceByPeriod,
    getMinBalanceByPeriod,
    getMaxBalanceByPeriod,
    getAvgExpensesByPeriod,
    getAvgRevenuesByPeriod,
    getAvgRemainsByPeriod,
    getYears,
    getMonths
} from '../data-fetcher/queries'

const years = getYears()
const months = getMonths()
const App = () => {
    const [year, setYear] = React.useState(2019);
    const handleChangeYear = (event: React.ChangeEvent<{ value: unknown }>) => {
        setYear(event.target.value as number);
    };

    const [month, setMonth] = React.useState(4);
    const handleChangeMonth = (event: React.ChangeEvent<{ value: unknown }>) => {
        setMonth(event.target.value as number);
    };

    return (
      <div>
        <Select title='year' options={years} value={year} handleChange={handleChangeYear}/>
        <Select title='month' options={months} value={month} handleChange={handleChangeMonth}/>
        <Card
            metric='Revenue'
            value={getRevenueByPeriod({year: year, month: month})}
            avg={getAvgRevenuesByPeriod({year: year})}
        />
        <Card
            metric='Expense'
            value={getExpenseByPeriod({year: year, month: month})}
            avg={getAvgExpensesByPeriod({year: year})}
        />
        <Card
            metric='Remain'
            value={getRemainByPeriod({year: year, month: month})}
            avg={getAvgRemainsByPeriod({year: year})}
        />
        <Card
            metric='Balance'
            value={getTotalBalanceByPeriod({year: year, month: month})}
            min={getMinBalanceByPeriod({year: year, month: month})}
            max={getMaxBalanceByPeriod({year: year, month: month})}
        />
    </div>
   )
}

export default App