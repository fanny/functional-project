const template = document.createElement('template');
template.innerHTML =
`
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

  get label() {
    return this.getAttribute('label');
  }

  static get observedAttributes() {
    return ['label']
  }

  attributeChangedCallback(name, oldVal, newVal) {
    this.render()
  }

  render() {
    this.$table.innerHTML += this.label
  }
}

window.customElements.define('custom-table', Table);