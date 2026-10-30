module Z.CLM.Stats.Season
  ( ClmSeason'Props
  , seasonId'active
  , seasonId'fromDateTime
  , seasonId'props
  , seasonId'slug
  , seasonId'title
  , seasonId'year
  , tz'chicago
  ) where

import Z.Prelude

tz'chicago :: Maybe String
tz'chicago = Just "America/Chicago"

seasonId'fromDateTime :: DateTime -> Int
seasonId'fromDateTime dt = periodMonthContrib + periodYearContrib
  where
  periodYearContrib = (((dateTime'year dt tz'chicago) - 2022) * 3) + 2
  periodMonthContrib = floor $ toNumber (dateTime'month'i0 dt tz'chicago) / 4.0

seasonId'active :: forall x. x @@> Int
seasonId'active = x'now <#> seasonId'fromDateTime

type ClmSeason'Props =
  { year :: Int
  , title :: String
  , slug :: String
  }

clmSeasonProps :: Int -> String -> String -> ClmSeason'Props
clmSeasonProps year title slug = { year, title, slug }

legacySeasonProps :: HashMap Int ClmSeason'Props
legacySeasonProps = hm'fromFoldable
  [ 2 /\ clmSeasonProps 2022 "2022 Spring" "chicago-2022-spring"
  , 3 /\ clmSeasonProps 2022 "2022 Spring" "chicago-2022-summer"
  , 4 /\ clmSeasonProps 2022 "2022 Spring" "chicago-2022-fall"
  , 5 /\ clmSeasonProps 2023 "2023 Act I" "chicago-2023-1"
  ]

seasonId'props :: Int -> ClmSeason'Props
seasonId'props seasonId =
  may'or_ generateProps $ hm'lookup seasonId legacySeasonProps
  where
  generateProps _ =
    let
      year = 2021 + (inc seasonId / 3)
      actNum = inc $ inc seasonId `mod` 3
      title = year <+> "Act" <+> int'romanNumeral actNum
      slug = "chicago" <-> year <-> actNum
    in
      clmSeasonProps year title slug

seasonId'year :: Int -> Int
seasonId'year = _.year <<< seasonId'props

seasonId'title :: Int -> String
seasonId'title = _.title <<< seasonId'props

seasonId'slug :: Int -> String
seasonId'slug = _.slug <<< seasonId'props