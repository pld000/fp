// 16.1
let notDivisible (n,m) = m % n = 0

let rec primeTest(n, m) =
    match (n, m) with
    | (0, _) | (1, _) -> true
    | (n, m) -> (not (notDivisible(n, m))) && primeTest(n - 1, m)

// 16.2
let prime n =
    match n with
    | 0 | 1 -> false
    | 2 -> true
    | n -> primeTest(n-1, n)
