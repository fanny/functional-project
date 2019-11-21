import '../custom-selector/index.js'
import { renderRows } from './helpers'
const template = document.createElement('template');
template.innerHTML = `
<style>
@import url('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css');

th, td {
  font-size: 14px;
}

</style>
<div class="bs-example container" data-example-id="striped-table">
<table class="table">
<thead>
  <tr>
    <custom-selector options=${JSON.stringify([2017, 2018, 2019])}></custom-selector>
    <custom-selector options=${JSON.stringify([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])}></custom-selector>
    <th scope="col" class="text-muted">Doc id</th>
    <th scope="col" class="text-muted">Date</th>
    <th scope="col" class="text-muted">Text Identifier</th>
    <th scope="col" class="text-muted">Value</th>
    <th scope="col" class="text-muted">Description</th>
    <th scope="col" class="text-muted">Types</th>
  </tr>
</thead>
<tbody>
</tbody>
</table>
</div>
`

class Table extends HTMLElement {
  constructor() {
    super()
    this._shadowRoot = this.attachShadow({ mode: 'open' })
    this._shadowRoot.appendChild(template.content.cloneNode(true))

    this.$table = this._shadowRoot.querySelector('table')
    this.$selector = this._shadowRoot.querySelector('selector')
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