-- | [現在時刻が関わるユニットテストから、テスト容易性設計を学ぶ](https://t-wada.hatenablog.jp/entry/design-for-testability)
module DesignForTestability where

import           Data.Time.Clock     (UTCTime, getCurrentTime)
import           Data.Time.LocalTime (LocalTime (localTimeOfDay),
                                      TimeOfDay (todHour),
                                      TimeZone (TimeZone, timeZoneMinutes, timeZoneName, timeZoneSummerOnly),
                                      utcToLocalTime)

-- | 「現在時刻」に応じて、挨拶の内容を下記のようにそれぞれ返す機能
-- （タイムゾーンは Asia/Tokyo とする）
--
-- - 朝（05:00:00以上 12:00:00未満）の場合、「おはようございます」と返す
-- - 昼（12:00:00以上 18:00:00未満）の場合、「こんにちは」と返す
-- - 夜（18:00:00以上 05:00:00未満）の場合、「こんばんは」と返す
printGreeting :: IO String
printGreeting = printGreetingI18n Ja

localHour :: TimeZone -> UTCTime -> Int
localHour tz = todHour . localTimeOfDay . utcToLocalTime tz

jst :: TimeZone
jst = TimeZone { timeZoneMinutes = 9 * 60, timeZoneSummerOnly = False, timeZoneName = "JST" }

-- |
-- >>> between 0 10 3
-- True
-- >>> between 0 10 10
-- False
-- >>> between 0 10 0
-- True
between :: Ord a => a -> a -> a -> Bool
between l u x = l <= x && x < u

data Language = Ja | En

data Greeting = Greeting { morning :: String, noon :: String, night :: String }

greeting :: Language -> Greeting
greeting Ja =
  Greeting
  { morning = "おはようございます"
  , noon = "こんにちは"
  , night = "こんばんは"
  }
greeting En =
  Greeting
  { morning = "Good Morning"
  , noon = "Good Afternoon"
  , night = "Good Evening"
  }

-- |
-- 「現在時刻」と「ロケール」に応じて、挨拶の内容を下記のようにそれぞれ返す機能を作成したい。
-- （ただし、タイムゾーンは Asia/Tokyo のままとする）
--
-- ロケールが 'ja' の場合
-- - 朝（05:00:00以上 12:00:00未満）の場合、「おはようございます」と返す
-- - 昼（12:00:00以上 18:00:00未満）の場合、「こんにちは」と返す
-- - 夜（18:00:00以上 05:00:00未満）の場合、「こんばんは」と返す
-- ロケールが 'en' の場合
-- - 朝（05:00:00以上 12:00:00未満）の場合、「Good Morning」と返す
-- - 昼（12:00:00以上 18:00:00未満）の場合、「Good Afternoon」と返す
-- - 夜（18:00:00以上 05:00:00未満）の場合、「Good Evening」と返す
printGreetingI18n :: Language -> IO String
printGreetingI18n l = greetI18n l . localHour jst <$> getCurrentTime

-- |
-- >>> putStr $ greetI18n Ja 5
-- おはようございます
-- >>> putStr $ greetI18n Ja 13
-- こんにちは
-- >>> putStr $ greetI18n Ja 0
-- こんばんは
-- >>> putStr $ greetI18n En 5
-- Good Morning
-- >>> putStr $ greetI18n En 13
-- Good Afternoon
-- >>> putStr $ greetI18n En 0
-- Good Evening
greetI18n :: Language -> Int -> String
greetI18n l h
  | between  5 12 h                  = morning g
  | between 12 18 h                  = noon g
  | between 18 24 h || between 0 5 h = night g
  | otherwise                        = undefined
  where
    g = greeting l
