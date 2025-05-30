// 42.3
let rec allSubsets n k =
    if k = 0 then
        Set.singleton Set.empty
    elif n < k then
        Set.empty
    elif n = k then
        Set.singleton (Set.ofList [1..n])
    else
        let withN = allSubsets (n-1) (k-1) |> Set.map (Set.add n)
        let withoutN = allSubsets (n-1) k
        Set.union withN withoutN
