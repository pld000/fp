// 34.1
let rec upto n =
    if n <= 0 then []
    elif n = 1 then [1]
    else upto (n-1) @ [n]


// 34.2
let rec dnto n =
    if n <= 0 then []
    elif n = 1 then [1]
    else n :: dnto(n-1)

// 34.3
let evenn n =
    let rec even m =
        if n = 1 then [0]
        elif m <= 1 then []
        elif m = n then 0 :: 2 :: even (m-1)
        else (2 * n - 2 * (m - 1)) :: even (m - 1)
    even n

// let rec evenn n =
//     if n <= 1 then []
//     elif n = 2 then [2]
//     elif n%2=0 then evenn (n-1) @ [n]
//     else evenn(n-1)
