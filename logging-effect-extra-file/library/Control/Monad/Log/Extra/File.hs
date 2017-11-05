{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Log.Extra.File
  ( -- * Getting Started
    -- $intro

    -- ** Quickstart using file info and severity
    -- $quickStartFileInfoAndSeverity

    -- ** Quickstart using only file info
    -- $quickStartFileInfo

    -- * Convenience logging combinators (TH)
    -- $convenience

    -- ** With severity
    logEmergencyTH
  , logAlertTH
  , logCriticalTH
  , logErrorTH
  , logWarningTH
  , logNoticeTH
  , logInformationalTH
  , logDebugTH
    -- ** Without severity
  , logMessageTH

    -- * Message transformers
    -- ** File info
  , WithFile(..)
  , renderWithFile

    -- * Utilities
  , logSeverityMessageTH
  , liftLoc

    -- * Re-exports
  , Loc(..)
  ) where

import Control.Monad.Log (WithSeverity(..), Severity(..))
import qualified Control.Monad.Log as Log
import Language.Haskell.TH (Exp, Loc(..), Q)
import Language.Haskell.TH.Syntax (Lift(lift))
import qualified Language.Haskell.TH.Syntax as TH
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as Pretty

-- | Generates a function that logs an 'Emergency' message with info from the
-- source file.
--
-- > $(logEmergencyTH) "GAH! All systems are down!!!"
logEmergencyTH :: Q Exp
logEmergencyTH = logSeverityMessageTH Emergency

-- | Generates a function that logs an 'Alert' message with info from the
-- source file.
--
-- > $(logAlertTH) "Red alert!"
logAlertTH :: Q Exp
logAlertTH = logSeverityMessageTH Alert

-- | Generates a function that logs a 'Critical' message with info from the
-- source file.
--
-- > $(logCriticalTH) "Critical hit!"
logCriticalTH :: Q Exp
logCriticalTH = logSeverityMessageTH Critical

-- | Generates a function that logs an 'Error' message with info from the
-- source file.
--
-- > $(logErrorTH) "Errors abound!"
logErrorTH :: Q Exp
logErrorTH = logSeverityMessageTH Error

-- | Generates a function that logs a 'Warning' message with info from the
-- source file.
--
-- > $(logWarningTH) "Cargo number 2331 has commandeered the vessel"
logWarningTH :: Q Exp
logWarningTH = logSeverityMessageTH Warning

-- | Generates a function that logs a 'Notice' message with info from the
-- source file.
--
-- > $(logNoticeTH) "Heads up, but it's no biggie."
logNoticeTH :: Q Exp
logNoticeTH = logSeverityMessageTH Notice

-- | Generates a function that logs an 'Informational' message with info from the
-- source file.
--
-- > $(logInformationalTH) "Does anyone read these?"
logInformationalTH :: Q Exp
logInformationalTH = logSeverityMessageTH Informational

-- | Generates a function that logs a 'Debug' message with info from the
-- source file.
--
-- > $(logDebugTH) "Sleuthing with log messages..."
logDebugTH :: Q Exp
logDebugTH = logSeverityMessageTH Debug

-- | Generates a function that logs a message with info from the
-- source file.
--
-- > $(logMessageTH) "Burn after reading."
logMessageTH :: Q Exp
logMessageTH = [|Log.logMessage . WithFile $(TH.qLocation >>= liftLoc)|]

--------------------------------------------------------------------------------
-- | Add \"File\" information to a log message.
data WithFile a = WithFile
  { msgLoc      :: Loc -- ^ Retrieve the file location info.
  , discardFile :: a   -- ^ View the underlying message.
  } deriving (Eq, Ord, Show, Functor, Traversable, Foldable) -- no Read instance (Loc)

-- | Given a way to render the underlying message @a@, render a message with its
-- file info.
--
-- >>> let loc = Loc "SomeFile.hs" "some-package" "SomeModule" (1, 1) (1, 1)
-- >>> renderWithFile id (WithFile loc "Some message")
-- [SomeModule:1] Some message
renderWithFile :: (a -> Doc) -> (WithFile a -> Doc)
renderWithFile k (WithFile (Loc {loc_module, loc_start}) a) = result where
  result = Pretty.brackets fileInfo Pretty.<+> rest
  fileInfo = Pretty.hcat (Pretty.punctuate Pretty.colon fileInfoList)
  fileInfoList = [Pretty.pretty loc_module, Pretty.pretty (fst loc_start)]
  rest = Pretty.align (k a)

-- | Generates a function that logs a message with the given 'Severity' and
-- info from the source file.
logSeverityMessageTH :: Severity -> Q Exp
logSeverityMessageTH severity =
  [|Log.logMessage . WithSeverity $(lift severity)
                   . WithFile $(TH.qLocation >>= liftLoc)|]

-- | Lift a location into an 'Exp'.
liftLoc :: Loc -> Q Exp
liftLoc (Loc {loc_filename, loc_package, loc_module, loc_start, loc_end}) =
  [|Loc $(lift loc_filename)
        $(lift loc_package)
        $(lift loc_module)
        ($(lift (fst loc_start)), $(lift (snd loc_start)))
        ($(lift (fst loc_end)), $(lift (snd loc_end)))|]

instance Lift Severity where
  lift Emergency     = [|Emergency|]
  lift Alert         = [|Alert|]
  lift Critical      = [|Critical|]
  lift Error         = [|Error|]
  lift Warning       = [|Warning|]
  lift Notice        = [|Notice|]
  lift Informational = [|Informational|]
  lift Debug         = [|Debug|]

{- $intro

@logging-effect-extra-file@ supplements [logging-effect](https://github.com/ocharles/logging-effect)
with TH splices that capture file information.

-}

{- $quickStartFileInfo

@
testAppFileOnly :: 'Log.MonadLog' ('WithFile' 'Pretty.Doc') m => m ()
testAppFileOnly = $('logMessageTH') "Heyo!!!"
@

-}

{- $quickStartFileInfoAndSeverity

@
testAppFileAndSeverity :: 'Log.MonadLog' ('WithSeverity' ('WithFile' 'Doc')) m => m ()
testAppFileAndSeverity = do
  $('logEmergencyTH') "GAH! All systems are down!!!"
  $('logAlertTH') "Red alert!"
  $('logCriticalTH') "Critical hit!"
  $('logErrorTH') "Errors abound!"
  $('logWarningTH') "Cargo number 2331 has commandeered the vessel"
  $('logNoticeTH') "Heads up, but it's no biggie."
  $('logInformationalTH') "Does anyone read these?"
  $('logDebugTH') "Sleuthing with log messages..."
@

-}

{- $convenience

@logging-effect-extra-file@ provides combinators for:

* adding file info to messages (module name and line number)
* adding both file info and severity to messages

In the former case, 'WithFile' will be at the outer-most level of your log
message stack.  In the latter case, 'WithSeverity' will be at the outer-most
level of your log message stack, wrapping 'WithFile'.

The package makes no assumptions on what is inside your log messages though.
There is a @logXTH@ combinator for each level in 'Severity'.

-}
