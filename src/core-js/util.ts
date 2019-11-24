const head = (values: any[]) => values[0]

const last = (values: any[]) => values.slice(-1)[0]

const sum = (numbers: number[]) => (
  numbers ?
  numbers.length && 
  numbers.reduce(
    (accumulator, current) => accumulator + current
  ): 0
)

const average = (numbers: number[]) => (
  numbers.length && (sum(numbers) / numbers.length)
)

const reduceGroups = (a: any) => {
  const keys = Object.keys(a)
  return keys.map(key => sum(a[key]))
}

const groupBy = (valueFn: any, keyFn: any, list: any[])=> {
  return list.reduce((acc, current) => {
    const key = keyFn(current)
    acc[key] = valueFn(acc.hasOwnProperty(key)? acc[key]: [], current)
    return acc
  }, {})
}

const flatten = (list:any) => {
  return list.reduce((acc:any, val:any) => (
    Array.isArray(val) ? 
      acc.concat(flatten(val)):
      acc.concat(val), []
    )
  )
}

const cumulativeSum = ((sum:number) => (value:number) => sum += value)

const min = (list: any) => Math.min(...list)

const max = (list: any) => Math.max(...list)

export {
  head,
  last,
  sum,
  average,
  reduceGroups,
  groupBy,
  flatten,
  cumulativeSum,
  min,
  max
}
