{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Log (MonadLog)
import qualified Control.Monad.Log as Log
import Control.Monad.Log.Extra.File (WithFile)
import qualified Control.Monad.Log.Extra.File as Log
import qualified System.IO as IO
import Data.Text.Prettyprint.Doc (Doc)

app :: MonadLog (WithFile (Doc ann)) m => m ()
app = do
  $(Log.logMessageTH) "GAH! All systems are down!!!"
  $(Log.logMessageTH) "Red alert!"
  $(Log.logMessageTH) "Critical hit!"
  $(Log.logMessageTH) "Errors abound!"
  $(Log.logMessageTH) "Cargo number 2331 has commandeered the vessel"
  $(Log.logMessageTH) "Heads up, but it's no biggie."
  $(Log.logMessageTH) "Does anyone read these?"
  $(Log.logMessageTH) "Sleuthing with log messages..."

main :: IO ()
main = Log.withFDHandler Log.defaultBatchingOptions IO.stdout 0.4 80 $ \stdoutHandler ->
  Log.runLoggingT app (stdoutHandler . Log.renderWithFile id)
