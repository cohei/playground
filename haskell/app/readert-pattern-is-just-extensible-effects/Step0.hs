module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Prelude hiding (log)

-- 1 Framework

type App context = ReaderT context IO

class Has e context where
  get :: context -> e

-- 2 Effect

data Logging = Logging {_log :: String -> IO ()}

log :: (Has Logging context) => String -> App context ()
log message = do
  f <- asks $ _log . get
  liftIO $ f message

-- 3 Implementation

data AppContext = AppContext {_Logging :: Logging}

instance Has Logging AppContext where
  get = _Logging

myContext :: AppContext
myContext = AppContext {_Logging = Logging {_log = putStrLn}}

-- 4 Application

myApp :: (Has Logging context) => App context ()
myApp = log "Hello ReaderT pattern"

main :: IO ()
main = runReaderT myApp myContext
