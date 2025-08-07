module Devices.Lock where

import Control.Exception

LockStatus
  = Unlocked
  | Locked
  | Error
  | Unknown
  deriving (Show, Eq)

class Monad m => Lock m l where
  unlock :: l -> m Either(LockError, LockStatus)
  lock :: l -> m Either(LockError, LockStatus)
  status ::l -> LockStatus

data LockError
  = Bind
  | Unreachable
  | Uninit
  deriving (Show)

instance Exception LockError

 
