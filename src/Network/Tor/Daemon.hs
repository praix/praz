module Network.Tor.Daemon where

import Prelude as P

run :: IO ()
run = do 
  P.putStrLn "running the Tor daemon..."