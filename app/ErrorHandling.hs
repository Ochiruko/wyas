module ErrorHandling
  ( module ErrorHandling
  , module Control.Monad.Except
  ) where

import Control.Monad.Except

import Datatypes

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "errors cannot be extracted"
