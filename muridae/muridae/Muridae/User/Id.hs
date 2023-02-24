module Muridae.User.Id (UserId (..)) where

import Data.Int (Int64)

newtype UserId = UserId Int64
  deriving stock (Eq, Show)
