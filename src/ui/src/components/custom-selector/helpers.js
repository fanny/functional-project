const renderOptions = (options) => {
  return JSON.parse(options).map(
    option => {console.log(option); return `<option value=${option}>${option}</option>`}
  )
}

export {
  renderOptions
}