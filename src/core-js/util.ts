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

const _zipWith: any = (fn: Function, a: any[], b: any[], newArray: any[]) => {
  const index = newArray.length;
  if(a.length == index || b.length == index)
    return newArray
  
  newArray.push(fn(a[index], b[index]))
  return _zipWith(fn, a, b, newArray)
}

const zipWith = (fn: Function, a: any[], b: any[]) => {
  return _zipWith(fn, a, b, []);
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
