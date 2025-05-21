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
        if m <= 0 then []
        else (2 * n - 2 * (m - 1)) :: even (m - 1)
    even n

// let rec evenn n =
//     if n <= 1 then []
//     elif n = 2 then [2]
//     elif n%2=0 then evenn (n-1) @ [n]
//     else evenn(n-1)
