module Control.Monad.Log.Extra
  ( -- * Batteries
    module Control.Monad.Log
  , module Control.Monad.Log.Extra.File
  , module Control.Monad.Log.Extra.Handler

    -- * Re-exports
  , module Data.Text.Prettyprint.Doc
  ) where

import Control.Monad.Log
import Control.Monad.Log.Extra.File
import Control.Monad.Log.Extra.Handler
import Data.Text.Prettyprint.Doc (Doc)
