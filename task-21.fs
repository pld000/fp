let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1

// 50.2.1
let fac_seq n = seq {
    for i in 0 .. n do yield factorial i
}

// 50.2.2
let seq_seq n = seq {
    for i in 0 .. n do
        if i = 0 then yield 0
        else yield! seq [-1 * i; i]
}
