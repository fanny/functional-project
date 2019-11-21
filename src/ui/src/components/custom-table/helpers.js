const renderRows = (data) => {
  return JSON.parse(data).map(
    row => `
      <tr>
        <th class="mb-0">${row.docID}</th>
        <td class="mb-0">${row.date.year}</td>
        <td class="mb-0">${row.textIdentifier}</td>
        <td class="mb-0">${row.value}</td>
        <td class="mb-0">${row.description}</td>
        <td class="mb-0">${row.transactionTypes}</td>
      </tr>`
  )
}

export {
  renderRows
}