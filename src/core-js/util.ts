const head = (values: any[]) => values[0]

const last = (values: any[]) => values.slice(-1)[0]

const sum = (numbers: number[]) => (
  numbers.length && 
  numbers.reduce(
    (accumulator, current) => accumulator + current
  )
)

const average = (numbers: number[]) => (
  numbers.length && (sum(numbers) / numbers.length)
)


const zipWith = (fn: Function, a: any, b: any) => {
  const keys = new Set(Object.keys(a).concat(Object.keys(b)))
  const listKeys = Array.from(keys)
  return listKeys.map(key => fn(sum(a[key]), sum(b[key])))
}

const groupBy = (valueFn: any, keyFn: any, list: any[])=> {
  return list.reduce((acc, current) => {
    const key = keyFn(current)
    acc[key] = valueFn(acc.hasOwnProperty(key)? acc[key]: [], current)
    return acc
  }, {})
}


const flatten = (list:any) => {
  return list.reduce((acc:any, val:any) => Array.isArray(val) ? acc.concat(flatten(val)) : acc.concat(val), [])
}

export {
  head,
  last,
  sum,
  average,
  zipWith,
  groupBy,
  flatten
}
