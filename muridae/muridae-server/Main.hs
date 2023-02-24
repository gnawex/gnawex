module Main (module Main) where

import Muridae.API.Server qualified as Server

main :: IO ()
main = Server.runMuridae
