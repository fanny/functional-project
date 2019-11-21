import { renderOptions } from './helpers'
const template = document.createElement('template');
template.innerHTML = `
<style>
@import url('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css');
</style>
<select class="btn btn-primary dropdown-toggle">
</select>
`
class Selector extends HTMLElement {
  constructor() {
    super()
    this._shadowRoot = this.attachShadow({ mode: 'open' })
    this._shadowRoot.appendChild(template.content.cloneNode(true))

    this.$selector = this._shadowRoot.querySelector('select')
  }

  get options() {
    return this.getAttribute('options');
  }

  static get observedAttributes() {
    return ['options']
  }

  set options(value) {
    this.setAttribute('options', value);
  }

  attributeChangedCallback() {
    this.render()
  }

  render() {
    renderOptions(this.options).forEach(option => this.$selector.innerHTML += option)
  }
}

window.customElements.define('custom-selector', Selector);
