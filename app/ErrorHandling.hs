module ErrorHandling where

import Control.Monad.Except

import Datatypes

type ThrowsError = Either LispError
