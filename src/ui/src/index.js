import './components/custom-table/index.js'
import filterByPeriod from './data-fetcher/queries'

const template = document.createElement('template');
template.innerHTML = `
  <style>
    :host {
      font-family: sans-serif;
    }
  </style>
  <div>
    <h1>Web Components</h1>
    <custom-table></custom-table>
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