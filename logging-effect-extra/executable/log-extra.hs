{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Log.Extra (Doc, WithFile, MonadLog, WithSeverity)
import qualified Control.Monad.Log.Extra as Log

app :: MonadLog (WithSeverity (WithFile Doc)) m => m ()
app = do
  $(Log.logEmergencyTH) "GAH! All systems are down!!!"
  $(Log.logAlertTH) "Red alert!"
  $(Log.logCriticalTH) "Critical hit!"
  $(Log.logErrorTH) "Errors abound!"
  $(Log.logWarningTH) "Cargo number 2331 has commandeered the vessel"
  $(Log.logNoticeTH) "Heads up, but it's no biggie."
  $(Log.logInfoTH) "Does anyone read these?"
  $(Log.logDebugTH) "Sleuthing with log messages..."

main :: IO ()
main = Log.withStdoutHandler $ \stdoutHandler ->
  Log.runLoggingT app (Log.iso8601PlusHandler stdoutHandler . Log.renderWithSeverity (Log.renderWithFile id))
