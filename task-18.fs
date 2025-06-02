// 47.4.1
let f n =
    if n = 0 then
        1
    else
        let x = ref n
        let result = ref 1
        while ! x > 0 do
            result := ! result * ! x
            x := ! x - 1
        !result

// 47.4.2
let fibo n =
    if n = 0 then 0
    elif n = 1 then 1
    else
        let x = ref 0
        let first = ref 0
        let second = ref 1
        let result = ref 0
        while !x <= (n-2) do
            result := !first + !second
            first := !second
            second := !result
            x := !x + 1
        !result
