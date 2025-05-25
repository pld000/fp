// 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | head :: tail -> if p head then head + sum (p, tail) else sum (p, tail)

// 40.2.1
let rec count (xs, n) =
    match xs with
    | [] -> 0
    | head :: tail -> if head = n then 1 + count (tail, n) elif head > n then 0 else count (tail, n)

// 40.2.2
let rec insert (xs, n) =
    match xs with
    | [] -> [n]
    | head :: tail -> if head >= n then n :: head :: tail else head :: insert (tail, n)

// Utils
let rec remove (xs, n) =
   match xs with
    | [] -> []
    | head :: tail -> if head = n then tail else head :: remove (tail, n)

// 40.2.3
let rec intersect (xs1, xs2) =
    match (xs1,xs2) with
    | ([], []) -> []
    | (tail1, []) -> []
    | ([], tail2) -> []
    | (head1 :: tail1, tail2) -> if count(tail2, head1) > 0 then head1 :: intersect (tail1, remove(tail2, head1)) else intersect (tail1, tail2)

// 40.2.4
let rec plus (xs1, xs2) =
    match (xs1,xs2) with
    | ([], []) -> []
    | (tail1, []) -> tail1
    | ([], tail2) -> tail2
    | (head1 :: tail1, head2 :: tail2) -> if head1 <= head2 then head1 :: head2 :: plus (tail1, tail2) else head2 :: head1 :: plus (tail1, tail2)

// 40.2.5
let rec minus (xs1, xs2) =
    match (xs1,xs2) with
    | ([], []) -> []
    | (tail1, []) -> []
    | ([], tail2) -> []
    | (head1 :: tail1, tail2) -> if count(tail2, head1) > 0 then minus (tail1, remove(tail2, head1)) else head1 :: minus (tail1, tail2)


// 40.3.1
let rec smallest = function
    | [] -> None
    | [x] -> Some x
    | head1 :: head2 :: tail -> if head1 <= head2 then smallest(head1 :: tail) else smallest(head2 :: tail)

// 40.3.2
let rec delete (n, xs) =
   match xs with
    | [] -> []
    | head :: tail -> if head = n then tail else head :: delete (n, tail)

// 40.3.3
let rec sort = function
    | [] -> []
    | [x] -> [x]
    | head :: tail ->
        let minTail = Option.get (smallest tail)
        if head <= minTail
        then
            head :: sort tail
        else
            let nextTail = delete (minTail, tail)
            minTail :: sort (head :: nextTail)

// 40.4
let revrev lst =
    let rec reverseLst = function
        | [] -> []
        | head :: tail -> List.rev head :: reverseLst tail
    List.rev (reverseLst lst)


