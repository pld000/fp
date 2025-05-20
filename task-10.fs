type TimeOfDay = { hours: int; minutes: int; f: string }
let (am, pm) = ("AM", "PM");

let isLaterPeriod (x: string, y: string) =
    x <> y && x = pm

let isTimeValid (x: TimeOfDay) =
    let {hours = xHours; minutes = xMinutes; f = xF} = x
    xHours >=0 && xHours <= 11 && xMinutes >= 0 && xMinutes <= 59 && (xF = am || xF = pm)

let (.>.) (x:TimeOfDay) (y:TimeOfDay) =
    let {hours = xHours; minutes = xMinutes; f = xF} = x
    let {hours = yHours; minutes = yMinutes; f = yF} = y
    if isTimeValid(x) && isTimeValid(y) then isLaterPeriod(xF, yF) || xHours > yHours || xMinutes > yMinutes
    else failwith "Invalid time"
