// 20.3.1
let vat n x =
    if n >= 0 && n <= 100
    then x + (x * float n / 100.0)
    else x

// 20.3.2
let unvat n x =
    if n >= 0 && n <= 100
    then x / (1.0 + float n / 100.0)
    else x

// 20.3.3
let min f =
    let rec search n =
        if f n = 0 then n
        else search (n + 1)
    search 1
