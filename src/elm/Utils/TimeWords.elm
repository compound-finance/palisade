module Utils.TimeWords exposing (distanceTimeInWords)

import Strings.Translations as Translations


type alias TimeElement =
    { singleName : String
    , pluralName : String
    , len : Int
    }


timeElements : Translations.Lang -> List TimeElement
timeElements userLanguage =
    [ { singleName = Translations.year userLanguage, pluralName = Translations.years userLanguage, len = 60 * 60 * 24 * 365 }
    , { singleName = Translations.day userLanguage, pluralName = Translations.days userLanguage, len = 60 * 60 * 24 }
    , { singleName = Translations.hour userLanguage, pluralName = Translations.hours userLanguage, len = 60 * 60 }
    , { singleName = Translations.minute userLanguage, pluralName = Translations.minutes userLanguage, len = 60 }
    , { singleName = Translations.second userLanguage, pluralName = Translations.seconds userLanguage, len = 1 }
    ]


distanceTimeInWords : Translations.Lang -> Int -> String
distanceTimeInWords userLanguage time =
    let
        str =
            timeElements userLanguage
                |> List.foldl
                    (\{ singleName, pluralName, len } ( currEls, remaining ) ->
                        if remaining > len && List.length currEls < 2 then
                            let
                                amt =
                                    remaining // len

                                name =
                                    if amt == 1 then
                                        singleName

                                    else
                                        pluralName

                                el =
                                    String.fromInt amt ++ " " ++ name
                            in
                            ( el :: currEls, remaining - amt * len )

                        else
                            ( currEls, remaining )
                    )
                    ( [], abs time )
                |> Tuple.first
                |> List.intersperse ", "
                |> List.foldl (++) ""
    in
    if time < 0 then
        Translations.time_ago userLanguage str

    else
        str
