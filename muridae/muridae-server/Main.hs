module Main (module Main) where

import MuridaeWeb.Server qualified as Server

main :: IO ()
main = Server.runMuridae
