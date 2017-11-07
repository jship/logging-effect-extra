{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Log (MonadLog, WithSeverity)
import qualified Control.Monad.Log as Log
import qualified Control.Monad.Log.Extra.Handler as Log
import Text.PrettyPrint.Leijen.Text (Doc)

app :: MonadLog (WithSeverity Doc) m => m ()
app = do
  Log.logEmergency "GAH! All systems are down!!!"
  Log.logAlert "Red alert!"
  Log.logCritical "Critical hit!"
  Log.logError "Errors abound!"
  Log.logWarning "Cargo number 2331 has commandeered the vessel"
  Log.logNotice "Heads up, but it's no biggie."
  Log.logInfo "Does anyone read these?"
  Log.logDebug "Sleuthing with log messages..."

main :: IO ()
main =
  Log.withStdoutHandler $ \stdoutHandler ->
  Log.withStderrHandler $ \stderrHandler ->
  Log.runLoggingT app (Log.routeHandler stdoutHandler stderrHandler id)
