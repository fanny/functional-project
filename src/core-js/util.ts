const head = (values: any[]) => values[0]

const last = (values: any[]) => values.slice(-1)[0]

const sum = (numbers: number[]) => (
  numbers.length && 
  numbers.reduce(
    (accumulator, current) => accumulator + current
  )
)

const median = (numbers: number[]) => (
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

export {
  head,
  last,
  sum,
  median,
  zipWith
}
