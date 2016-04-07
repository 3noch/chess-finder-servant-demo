module Main where

import System.Process (system)
import Twitch


main :: IO ()
main = defaultMain $ do
  "**/*.hs"
  |> \_ -> system $ "killall serve-here & stack build --fast && make"
