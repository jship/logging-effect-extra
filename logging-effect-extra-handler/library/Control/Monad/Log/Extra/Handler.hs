module Control.Monad.Log.Extra.Handler
  ( -- * Getting Started
    -- $intro

    -- ** Quickstart using stdout handler
    -- $quickStartStdoutHandler

    -- ** Quickstart using a timestamp handler
    -- $quickStartTimestampHandler

    -- ** Quickstart using routing handler
    -- $quickStartRoutingHandler

    -- ** Quickstart using routing handler with timestamps
    -- $quickStartRoutingHandlerWithTimestamps

    -- * Convenience handler combinators
    -- $convenience

    -- ** Timestamp handlers
    iso8601Handler
  , iso8601PlusHandler
  , rfc822Handler

    -- ** Routing handlers
  , routeHandler
  , dispatchHandler

    -- ** Shortcuts for stdout/stderr handlers
  , withStdoutHandler
  , withStderrHandler
  , withCustomStdoutHandler
  , withCustomStderrHandler

    -- * Utilities
  , customTimestampHandler
  , withCustomHandler
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Log (BatchingOptions, Handler, Severity(..), WithSeverity(..))
import qualified Control.Monad.Log as Log
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.IO (Handle)
import qualified System.IO as IO
import Text.PrettyPrint.Leijen.Text (Doc)

-- | Converts an existing handler into a handler that renders an ISO8601
-- (i.e. YYYY-MM-DDTHH:MM:SS) timestamp on every log message.
iso8601Handler :: (MonadIO m, MonadMask m) => Handler m Doc -> Handler m Doc
iso8601Handler = customTimestampHandler formatter where
  formatter = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

-- | Converts an existing handler into a handler that renders an ISO8601
-- (i.e. YYYY-MM-DDTHH:MM:SS with decimal point and fraction of second)
-- timestamp on every log message.
iso8601PlusHandler :: (MonadIO m, MonadMask m)
                   => Handler m Doc
                   -> Handler m Doc
iso8601PlusHandler = customTimestampHandler formatter where
  formatter = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q"

-- | Converts an existing handler into a handler that renders an RFC822
-- timestamp on every log message.
rfc822Handler :: (MonadIO m, MonadMask m) => Handler m Doc -> Handler m Doc
rfc822Handler = customTimestampHandler formatter where
  formatter = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat

-- | Converts an existing handler into a handler that renders a timestamp on
-- every log message. The timestamp is formatted via the input function.
customTimestampHandler :: (MonadIO m, MonadMask m)
                       => (UTCTime -> String)
                       -> Handler m Doc
                       -> Handler m Doc
customTimestampHandler formatter handler = \msg -> do
  msg' <- Log.timestamp msg
  handler (Log.renderWithTimestamp formatter id msg')

-- | Basic dispatch handler that routes 'Warning', 'Notice', 'Informational',
-- and 'Debug' messages to the first input handler and routes 'Emergency',
-- 'Alert', 'Critical', and 'Error' messages to the second input handler.
routeHandler :: (MonadIO m, MonadMask m)
             => Handler m Doc -- ^ The handler for non-error messages (i.e. stdout handler)
             -> Handler m Doc -- ^ The handler for error messages (i.e. stderr handler)
             -> (a -> Doc)    -- ^ How to render
             -> Handler m (WithSeverity a)
routeHandler stdoutHandler stderrHandler renderer = \msg ->
  let msg' = Log.renderWithSeverity renderer msg
      handler = case msgSeverity msg of
        Emergency     -> stderrHandler
        Alert         -> stderrHandler
        Critical      -> stderrHandler
        Error         -> stderrHandler
        Warning       -> stdoutHandler
        Notice        -> stdoutHandler
        Informational -> stdoutHandler
        Debug         -> stdoutHandler
   in handler msg'

-- | Basic dispatch handler that routes 'Warning', 'Notice', 'Informational',
-- and 'Debug' messages to the first input handler and routes 'Emergency',
-- 'Alert', 'Critical', and 'Error' messages to the second input handler.
-- This function is limiting as it assumes incoming messages are
-- 'WithSeverity' 'Doc' instead of the more general 'WithSeverity' 'a'.
dispatchHandler :: (MonadIO m, MonadMask m)
                => Handler m Doc -- ^ The handler for non-error messages (i.e. stdout handler)
                -> Handler m Doc -- ^ The handler for error messages (i.e. stderr handler)
                -> Handler m (WithSeverity Doc)
dispatchHandler stdoutHandler stderrHandler =
  routeHandler stdoutHandler stderrHandler id
{-# DEPRECATED dispatchHandler "dispatchHandler is deprecated in favor of routeHandler." #-}

-- | Convenience wrapper around 'Log.withFDHandler' for 'IO.stdout' with somewhat sensible defaults.
withStdoutHandler :: (MonadIO m, MonadMask m) => (Handler m Doc -> m a) -> m a
withStdoutHandler = withCustomStdoutHandler Log.defaultBatchingOptions 0.4 80

-- | Convenience wrapper around 'Log.withFDHandler' for 'IO.stderr' with somewhat sensible defaults.
withStderrHandler :: (MonadIO m, MonadMask m) => (Handler m Doc -> m a) -> m a
withStderrHandler = withCustomStderrHandler Log.defaultBatchingOptions 0.4 80

-- | Convenience wrapper around 'Log.withFDHandler' for 'IO.stdout'.
withCustomStdoutHandler :: (MonadIO m, MonadMask m)
                        => BatchingOptions
                        -> Float -- ^ The @ribbonFrac@ parameter to 'Pretty.renderPretty'
                        -> Int -- ^ The amount of characters per line. Lines longer than this will be pretty-printed across multiple lines if possible.
                        -> (Handler m Doc -> m a)
                        -> m a
withCustomStdoutHandler = withCustomHandler IO.stdout

-- | Convenience wrapper around 'Log.withFDHandler' for 'IO.stderr'.
withCustomStderrHandler :: (MonadIO m, MonadMask m)
                        => BatchingOptions
                        -> Float -- ^ The @ribbonFrac@ parameter to 'Pretty.renderPretty'
                        -> Int -- ^ The amount of characters per line. Lines longer than this will be pretty-printed across multiple lines if possible.
                        -> (Handler m Doc -> m a)
                        -> m a
withCustomStderrHandler = withCustomHandler IO.stderr

-- | Convenience wrapper around 'Log.withFDHandler' that enables partially
-- applying the 'Handle' as the first parameter.
withCustomHandler :: (MonadIO m, MonadMask m)
                  => Handle
                  -> BatchingOptions
                  -> Float -- ^ The @ribbonFrac@ parameter to 'Pretty.renderPretty'
                  -> Int -- ^ The amount of characters per line. Lines longer than this will be pretty-printed across multiple lines if possible.
                  -> (Handler m Doc -> m a)
                  -> m a
withCustomHandler handle options ribbonFrac width = Log.withFDHandler options handle ribbonFrac width

{- $intro

@logging-effect-extra-handle@ supplements [logging-effect](https://github.com/ocharles/logging-effect)
with convenience handler combinators.

In the quickstart examples, please assume the following is in scope:

@
app :: 'Log.MonadLog' ('WithSeverity' 'Doc') m => m ()
app = 'Log.logWarning' "Cargo number 2331 has commandeered the vessel"
@

-}

{- $quickStartStdoutHandler

@
main :: IO ()
main = 'withStdoutHandler' $ \stdoutHandler ->
  'Log.runLoggingT' app (stdoutHandler . 'Log.renderWithSeverity' id)
@

-}

{- $quickStartTimestampHandler

@
main :: IO ()
main = 'withStdoutHandler' $ \stdoutHandler ->
  'Log.runLoggingT' app ('iso8601Handler' stdoutHandler . 'Log.renderWithSeverity' id)
@

-}

{- $quickStartRoutingHandler

@
main :: IO ()
main =
  'withStdoutHandler' $ \stdoutHandler ->
  'withStderrHandler' $ \stderrHandler ->
  'Log.runLoggingT' app ('routeHandler' stdoutHandler stderrHandler 'id')
@

-}

{- $quickStartRoutingHandlerWithTimestamps

@
main :: IO ()
main =
  'withStdoutHandler' $ \stdoutHandler ->
  'withStderrHandler' $ \stderrHandler ->
  'Log.runLoggingT' app ('routeHandler' ('iso8601Handler' stdoutHandler) ('iso8601Handler' stderrHandler) 'id')
@

-}

{- $convenience

@logging-effect-extra-handler@ provides combinators for:

* producing timestamping handlers from existing handlers
* convenience handlers for 'IO.stdout' and 'IO.stderr'
* dispatching handler to route messages to non-error and error handlers

-}
