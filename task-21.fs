// 50.2.1
let fac_seq =
    seq {
        let rec factorial = function
            | 0 -> 1
            | n -> n * factorial (n - 1)

        let mutable i = 0
        while true do
            yield factorial i
            i <- i + 1
    }

// 50.2.2
let seq_seq =
    seq {
        yield 0
        let mutable n = 1
        while true do
            yield -n
            yield n
            n <- n + 1
    }
