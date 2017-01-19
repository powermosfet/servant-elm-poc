module Main where 

import Network.Wai.Handler.Warp
import System.Environment
import System.IO

import Config
import Api

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let config = fromEnvironment env 
    let port = configServerPort config
    run port =<< mkApp config
