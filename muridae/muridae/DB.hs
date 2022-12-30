{-# LANGUAGE OverloadedStrings #-}

module DB where

import DB.Types (MuridaeDB)
import Database.Beam (DatabaseSettings, defaultDbSettings)
import Database.Beam.Postgres (Postgres)

muridaeDB :: DatabaseSettings Postgres MuridaeDB
muridaeDB = defaultDbSettings
