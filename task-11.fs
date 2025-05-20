type F =
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let isTimeValid (x: TimeOfDay) =
    let {hours = xHours; minutes = xMinutes; f = xF} = x
    xHours >=0 && xHours <= 11 && xMinutes >= 0 && xMinutes <= 59 && (xF = AM || xF = PM)

let (.>.) (x:TimeOfDay) (y:TimeOfDay) =
    let {hours = xHours; minutes = xMinutes; f = xF} = x
    let {hours = yHours; minutes = yMinutes; f = yF} = y
    if isTimeValid(x) && isTimeValid(y) then
        match xF, yF with
            | PM, AM -> true
            | AM, PM -> false
            | _ -> xHours > yHours || xMinutes > yMinutes
    else failwith "Invalid time"
