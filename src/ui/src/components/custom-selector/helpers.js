const renderOptions = (options) => {
  return JSON.parse(options).map(
    option => `<option value=${option}>${option}</option>`
  )
}

export {
  renderOptions
}