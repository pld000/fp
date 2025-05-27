// 41.4.1
let list_filter f xs =
    List.foldBack (fun num lst -> if f num then num :: lst else lst) xs []

// 41.4.2
let sum (p, xs) =
    List.fold (fun acc num -> if p num then acc + num else acc) 0 xs

// 41.4.3
let revrev lst =
    let rec reverseLst = function
        | [] -> []
        | head :: tail -> List.fold (fun head tail -> tail::head) [] head :: reverseLst tail
    List.fold (fun head tail -> tail::head) [] (reverseLst lst)
