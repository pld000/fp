// 49.5.1
let even_seq = Seq.initInfinite (fun i -> i) |> Seq.filter (fun x -> x % 2 = 0)

// 49.5.2
let fac_seq = Seq.initInfinite (fun i ->
    let rec fac = function
        | 0 -> 1
        | a -> a * fac(a-1)
    fac i
    )

// 49.5.3
let seq_seq = Seq.unfold (fun x -> if x=0 then Some(0, -1) elif x<0 then Some(x, -1*x) else Some(x, -1*(x+1))) 0
