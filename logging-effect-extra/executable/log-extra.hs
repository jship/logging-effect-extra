{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Log (MonadLog, WithSeverity)
import qualified Control.Monad.Log as Log
import Control.Monad.Log.Extra (WithFile)
import qualified Control.Monad.Log.Extra as Log
import qualified Control.Monad.Log.Extra as Log ()
import qualified System.IO as IO
import Text.PrettyPrint.Leijen.Text (Doc)

app :: MonadLog (WithSeverity (WithFile Doc)) m => m ()
app = do
  $(Log.logEmergencyTH) "GAH! All systems are down!!!"
  $(Log.logAlertTH) "Red alert!"
  $(Log.logCriticalTH) "Critical hit!"
  $(Log.logErrorTH) "Errors abound!"
  $(Log.logWarningTH) "Cargo number 2331 has commandeered the vessel"
  $(Log.logNoticeTH) "Heads up, but it's no biggie."
  $(Log.logInformationalTH) "Does anyone read these?"
  $(Log.logDebugTH) "Sleuthing with log messages..."

main :: IO ()
main = Log.withFDHandler Log.defaultBatchingOptions IO.stdout 0.4 80 $ \stdoutHandler ->
  Log.runLoggingT app (stdoutHandler . Log.renderWithSeverity (Log.renderWithFile id))
