{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | [実戦での Scala: Cake パターンを用いた Dependency Injection (DI)](http://eed3si9n.com/ja/real-world-scala-dependency-injection-di/)
module DependencyInjection where

class OnOffDevice a m where
  on :: a -> m ()
  off :: a -> m ()

class SensorDevice a where
  isCoffeePresent :: a -> Bool

data Heater = Heater

instance OnOffDevice Heater IO where
  on _ = putStrLn "heater.on"
  off _ = putStrLn "heater.off"

data PotSensor = PotSensor

instance SensorDevice PotSensor where
  isCoffeePresent = const True

data Warmer p h = Warmer { potSensor :: p, heater :: h }

triggerWarmer :: (OnOffDevice h m, SensorDevice p) => Warmer p h -> m ()
triggerWarmer Warmer{potSensor, heater}
  | isCoffeePresent potSensor = on heater
  | otherwise                 = off heater

warmer :: Warmer PotSensor Heater
warmer = Warmer PotSensor Heater

-- |
-- >>> triggerWarmer' warmer
-- heater.on
triggerWarmer' :: Warmer PotSensor Heater -> IO ()
triggerWarmer' = triggerWarmer
