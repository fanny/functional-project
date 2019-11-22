import {
  filterByPeriod,
  getRevenueByPeriod,
  getExpenseByPeriod,
  getRemainsByPeriod,
  getTotalBalanceByPeriod,
  getMaxBalanceByPeriod,
  getMinBalanceByPeriod,
  getAvgRevenuesByPeriod,
  getAvgExpensesByPeriod,
  getAvgRemainsByPeriod
} from './data-fetcher/queries'

const template = document.createElement('template');
template.innerHTML = `
  <style>
    :host {
      font-family: sans-serif;
    }
  </style>
  <div>
    <h1>Web Components</h1>
    <span>${getRevenueByPeriod({year: 2017})}</span>
    <span>${getExpenseByPeriod({year: 2017})}</span>
    <span>${getRemainsByPeriod({year: 2017})}</span>
    <span>${getTotalBalanceByPeriod({year: 2017})}</span>
    <span>${getTotalBalanceByPeriod({year: 2017})}</span>
    <span>${getMaxBalanceByPeriod({year: 2017})}</span>
    <span>${getMinBalanceByPeriod({year: 2017})}</span>
    <span>${getAvgRevenuesByPeriod({year: 2017})}</span>
    <span>${getAvgExpensesByPeriod({year: 2017})}</span>
    <span>${getAvgRemainsByPeriod({year: 2017})}</span>
  </div>
`

class App extends HTMLElement {
  constructor() {
    super();
    this._shadowRoot = this.attachShadow({ mode: 'open' })
    this._shadowRoot.appendChild(template.content.cloneNode(true))

    this.$customTable = this._shadowRoot.querySelector('custom-table')
    this.$customTable.data = JSON.stringify(filterByPeriod({year: 2017}))
  }
}

window.customElements.define('my-app', App);