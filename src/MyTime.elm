module MyTime exposing (..)

import Time
import Iso8601


--add 30 days to given posix time
plusOneMoth: Time.Posix -> Time.Posix
plusOneMoth time =
    Time.millisToPosix <| (Time.posixToMillis time) + 2592000000


timeToHumanReadable: Time.Posix -> String
timeToHumanReadable time =
    String.fromInt (Time.toYear Time.utc time)
      ++ "-" ++
      monthToHumanReadable (Time.toMonth Time.utc time)
      ++ "-" ++
      String.fromInt (Time.toDay Time.utc time)


monthToHumanReadable: Time.Month -> String
monthToHumanReadable month =
    case month of
            Time.Jan -> "01"
            Time.Feb -> "02"
            Time.Mar -> "03"
            Time.Apr -> "04"
            Time.May -> "05"
            Time.Jun -> "06"
            Time.Jul -> "07"
            Time.Aug -> "08"
            Time.Sep -> "09"
            Time.Oct -> "10"
            Time.Nov -> "11"
            Time.Dec -> "12"

fromInputStringToTime: String -> Maybe Time.Posix
fromInputStringToTime stringTime =
    case Iso8601.toTime <| stringTime ++ "T00:00:00" of
        Ok time -> Just time
        Err err -> Nothing