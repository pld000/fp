// 48.4.1
let rec fibo1 n n1 n2 =
    if n <= 2 then
        n1 + n2
    else
        fibo1 (n-1) (n1 + n2) n1


// 48.4.2
let rec fibo2 n c =
    if n <= 2 then
        c 2
    else
        fibo2 (n - 1) (fun n1 -> n2 -> n1 + n2)
