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
    getAvgRemainsByPeriod
} from '../data-fetcher/queries'

const App = () => (
    <div>
        <Select/>
        <Select/>
        <Card
            metric='Revenue'
            value={getRevenueByPeriod({year: 2019, month: 4})}
            avg={getAvgRevenuesByPeriod({year: 2019})}
        />
        <Card
            metric='Expense'
            value={getExpenseByPeriod({year: 2019, month: 4})}
            avg={getAvgExpensesByPeriod({year: 2019})}
        />
        <Card
            metric='Remain'
            value={getRemainByPeriod({year: 2019, month: 4})}
            avg={getAvgRemainsByPeriod({year: 2019})}
        />
        <Card
            metric='Balance'
            value={getTotalBalanceByPeriod({year: 2019, month: 4})}
            min={getMinBalanceByPeriod({year: 2019, month: 4})}
            max={getMaxBalanceByPeriod({year: 2019, month: 4})}
        />
    </div>
)

export default App