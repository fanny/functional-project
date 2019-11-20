const template = document.createElement('template');
template.innerHTML = `
<table>
<thead>
  <tr>
    <th>DocID</th>
    <th>Date</th>
    <th>Text Identifier</th>
    <th>Value</th>
    <th>Types</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>1</td>
    <td>10/08/2000</td>
    <td>Heyy</td>
    <td>10</td>
    <td>SALDO CORRENTE</td>
  </tr>
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
    this.$table.querySelector('tbody').innerHTML += this.data
  }
}

window.customElements.define('custom-table', Table);