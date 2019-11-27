import React from 'react'
import Card from  './SummarizingCard/index'
import Select from  './Select/index'
import Table from  './Table/index'
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
    getCashFlow,
    getYears,
    getMonths
} from '../data-fetcher/queries'
import Box from '@material-ui/core/Box';


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
      <div style={{backgroundColor: '#eeeeee'}}>
        <Box display="flex" flexDirection="row" justifyContent="flex-end">
        <Select title='year' options={years} value={year} handleChange={handleChangeYear}/>
        <Select title='month' options={months} value={month} handleChange={handleChangeMonth}/>
        </Box>
        <Box display="flex" flexDirection="row" justifyContent="center">
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
        </Box>
        <Box display="flex" flexDirection="row" justifyContent="center">
        <Table rows={getCashFlow({year: year, month: month})}/>
        </Box>

    </div>
   )
}

export default App