import '../custom-selector/index.js'
import { renderRows } from './helpers'
const template = document.createElement('template');
template.innerHTML = `
<table>
<thead>
  <tr>
    <custom-selector options=${JSON.stringify([2017, 2018, 2019])}></custom-selector>
    <custom-selector options=${JSON.stringify([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])}></custom-selector>
    <th>DocID</th>
    <th>Date</th>
    <th>Text Identifier</th>
    <th>Value</th>
    <th>Description</th>
    <th>Types</th>
  </tr>
</thead>
<tbody>
</tbody>
</table>
`

class Table extends HTMLElement {
  constructor() {
    super()
    this._shadowRoot = this.attachShadow({ mode: 'open' })
    this._shadowRoot.appendChild(template.content.cloneNode(true))

    this.$table = this._shadowRoot.querySelector('table')
  }

  get data() {
    return this.getAttribute('data');
  }

  static get observedAttributes() {
    return ['data']
  }

  set data(value) {
    this.setAttribute('data', value);
  }

  attributeChangedCallback() {
    this.render()
  }

  render() {
    renderRows(this.data).forEach(row => this.$table.querySelector('tbody').innerHTML += row)
  }
}

window.customElements.define('custom-table', Table);