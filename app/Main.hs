{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Servant
import Data.Function ((&))
import Network.Wai.Handler.Warp (run)

type API = "counter" :> PostNoContent

makeServer :: Server API
makeServer = pure NoContent

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let application :: Application = makeServer & serve (Proxy @API)
    application & run 8000
    pure ()
