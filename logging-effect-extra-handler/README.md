# [logging-effect-extra-handler][]

## Synopsis

```haskell
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
  Log.runLoggingT app (Log.routeHandler (Log.iso8601Handler stdoutHandler)
                                        (Log.iso8601Handler stderrHandler)
                                        id)
```

## Usage via `stack`

``` sh
# Build the project.
stack build

# Run the `iso8601-handler` example
stack exec iso8601-handler

# Run the `iso8601plus-handler` example
stack exec iso8601plus-handler

# Run the `rfc822-handler` example
stack exec rfc822-handler

# Run the `route-handler-with-iso8601` example
stack exec route-handler-with-iso8601

# Run the `route-handler` example
stack exec route-handler
```

[logging-effect-extra-handler]: https://github.com/jship/logging-effect-extra
