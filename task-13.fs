//39.1
let rec rmodd = function
    | [] -> []
    | [x] -> []
    | x0 :: x1 :: tail -> x1 :: rmodd(tail)

// 39.2
let rec del_even = function
    | [] -> []
    | head :: tail -> if head % 2 > 0 then  head :: del_even tail else del_even tail

// 39.3
let rec multiplicity x xs =
    match xs with
    | [] -> 0
    | head :: tail -> if x = head then 1 + multiplicity x tail else multiplicity x tail

// 39.4
let rec lmodd = function
    | [] -> []
    | [x] -> [x]
    | x0 :: x1 :: tail -> x0 :: lmodd(tail)

let split lst = (lmodd lst, rmodd lst)

// 39.5
let rec zip (xs1,xs2) =
    match (xs1,xs2) with
    | ([], []) -> []
    | (xs1,xs2) when List.length xs1 <> List.length xs2 -> failwith "Wrong list length"
    | (head1 :: tail1, head2 :: tail2) -> (head1, head2) :: zip (tail1, tail2)
