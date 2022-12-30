module MuridaeWeb.Types where

import Data.Kind (Type)
import Effectful (Eff, IOE)
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader)
import Muridae.Environment (MuridaeEnv)
import Servant (ServerError)
import Effectful.Beam (DB)

type Handler' :: Type -> Type
type Handler' =
  Eff
    '[ Reader MuridaeEnv
     , DB
     , Error ServerError
     , IOE
     ]
