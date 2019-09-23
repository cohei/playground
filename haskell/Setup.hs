module Main (main) where

import           Distribution.Simple                (Args, defaultMainWithHooks,
                                                     preBuild, simpleUserHooks)
import           Distribution.Simple.Setup          (BuildFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Hpack                              (Verbose (Verbose),
                                                     defaultOptions, hpack)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = pbHpack }

pbHpack :: Args -> BuildFlags -> IO HookedBuildInfo
pbHpack _ _ = emptyHookedBuildInfo <$ hpack'

hpack' :: IO ()
hpack' = hpack Verbose defaultOptions
