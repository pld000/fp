// 23.4.1
let (silverInGold, copperInSilver) = (20, 12)

let convertToCopper (x : int * int * int) =
    let (xGold, xSilver, xCopper) = x
    xGold * silverInGold * copperInSilver + xSilver * copperInSilver + xCopper

let calcSilver (copperSum, copper) =
    if ((copperSum - copper) > 0) then ((copperSum - copper) / copperInSilver) % silverInGold else 0

let calcGold (copperSum, silver, copper) =
    if ((copperSum - copper - silver * copperInSilver) > 0) then (copperSum - copper - silver * copperInSilver) / (silverInGold * copperInSilver) else 0

let convertToThree (copperSum: int) =
    let copper = copperSum % copperInSilver
    let silver = calcSilver(copperSum, copper)
    let gold = calcGold(copperSum, silver, copper)
    (gold, silver, copper)

let (.+.) (x : int * int * int) (y : int * int * int) : int * int * int =
    convertToThree (convertToCopper x + convertToCopper y)


let (.-.) (x : int * int * int) (y : int * int * int) : int * int * int =
    let result = convertToCopper x - convertToCopper y
    if result <= 0 then (0, 0, 0) else convertToThree result

// 23.4.2

let (.+) (x: float * float) (y: float * float) =
    let (a, b) = x;
    let (c, d) = y;
    (a + c, b + d)

let (.-) (x: float * float) (y: float * float) =
    let (c, d) = y;
    let reverseNum = (-c, -d)
    x .+ reverseNum

let (.*) (x: float * float) (y: float * float) =
    let (a, b) = x;
    let (c, d) = y;
    (a * c - b * d, b * c + a * d)

let (./) (x: float * float) (y: float * float) =
    let (c, d) = y;
    let reverseNum = (c/(c**2 + d**2), -d/(c**2+d**2))
    x .* reverseNum
