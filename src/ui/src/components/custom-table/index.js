import { renderRows } from './helpers'
const template = document.createElement('template');
template.innerHTML = `
<table>
<thead>
  <tr>
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