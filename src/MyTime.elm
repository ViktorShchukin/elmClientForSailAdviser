module MyTime exposing (..)

import Time
import Iso8601

timeToHumanReadable: Time.Posix -> String
timeToHumanReadable time =
    String.fromInt (Time.toYear Time.utc time)
      ++ "-" ++
      monthToHumanReadable (Time.toMonth Time.utc time)
      ++ "-" ++
      String.fromInt (Time.toDay Time.utc time)
      ++ " (UTC)"

monthToHumanReadable: Time.Month -> String
monthToHumanReadable month =
    case month of
            Time.Jan -> "1"
            Time.Feb -> "2"
            Time.Mar -> "3"
            Time.Apr -> "4"
            Time.May -> "5"
            Time.Jun -> "6"
            Time.Jul -> "7"
            Time.Aug -> "8"
            Time.Sep -> "9"
            Time.Oct -> "10"
            Time.Nov -> "11"
            Time.Dec -> "12"

fromInputStringToTime: String -> Maybe Time.Posix
fromInputStringToTime stringTime =
    case Iso8601.toTime <| stringTime ++ "T00:00:00" of
        Ok time -> Just time
        Err err -> Nothing