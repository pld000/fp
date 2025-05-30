// 42.3
let rec allSubsets n k =
    if n = k then
        Set.ofList [Set.ofList [1 .. n]]
    elif k < 0 || k > n then
        Set.empty
    elif k = 0 then
        Set.ofList [Set.empty]
    elif n = 0 then
        Set.empty
    else
        let subsetsWithoutN = allSubsets (n - 1) (k - 1)
        let withN = subsetsWithoutN |> Set.map (fun subset -> Set.add n subset)
        let withoutN = allSubsets (n - 1) k
        Set.union withN withoutN
