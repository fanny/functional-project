import { renderOptions } from './helpers'
const template = document.createElement('template');
template.innerHTML = `
<select>
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
    console.log(this.options)
    renderOptions(this.options).forEach(option => this.$selector.innerHTML += option)
  }
}

window.customElements.define('custom-selector', Selector);


[2017, 2018, 2019]
[0,1,2,3,4,5,6,7,8,9,10,11]
