const renderRows = (data) => {
  return JSON.parse(data).map(
    row => `
      <tr>
        <td>${row.docID}</td>
        <td>${row.date.year}</td>
        <td>${row.textIdentifier}</td>
        <td>${row.value}</td>
        <td>${row.description}</td>
        <td>${row.transactionTypes}</td>
      </tr>`
  )
}

export {
  renderRows
}