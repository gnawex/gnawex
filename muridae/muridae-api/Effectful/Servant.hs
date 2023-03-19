module Effectful.Servant (runUVerb, throwUVerb) where

import Data.Kind (Type)
import Effectful (Eff, Effect)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Servant (HasStatus, IsMember, Union, respond)

--------------------------------------------------------------------------------
-- Adaptations of:
-- https://docs.servant.dev/en/stable/cookbook/uverb/UVerb.html#idiomatic-exceptions

runUVerb
  :: forall (xs :: [Type]) (es :: [Effect])
   . Eff (Error (Union xs) : es) (Union xs)
  -> Eff es (Union xs)
runUVerb = fmap (either id id) . runErrorNoCallStack @(Union xs)

throwUVerb
  :: forall (x :: Type) (xs :: [Type]) (es :: [Effect])
   . (HasStatus x, IsMember x xs)
  => x
  -> Eff (Error (Union xs) : es) (Union xs)
throwUVerb uverb = respond uverb >>= throwError @(Union xs)
