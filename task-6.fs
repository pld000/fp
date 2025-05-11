// 17.1
let rec pow = function
    | (s,0) -> ""
    | (s,n) -> s + pow(s, n-1)

// 17.2
let isIthChar (s:string, n:int, c:char) = s.[n] = c

// 17.3
let rec occFromIth(s:string, n:int, c:char) =
    match n with
        | n when n >= (String.length s) -> 0
        | n when s.[n] = c -> 1 + occFromIth(s, n + 1, c)
        | n when s.[n] <> c -> occFromIth(s, n + 1, c)
