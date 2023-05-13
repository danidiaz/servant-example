{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Servant
import Data.Function ((&))
import Network.Wai.Handler.Warp (run)
import Data.IORef
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic)

type API = "counter" :> NamedRoutes Counter

data Counter mode = Counter {
    increaseCounter :: mode :- PostNoContent,
    inspectCounter :: mode :- Get '[JSON] Int
} deriving stock Generic

makeServer :: IORef Int -> Server API
makeServer ref = Counter {
    increaseCounter = 
        do
        liftIO $ modifyIORef' ref (+1)
        pure NoContent,
    inspectCounter = do
        liftIO $ readIORef ref
}

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    ref <- newIORef 0
    let application :: Application = makeServer ref & serve (Proxy @API)
    application & run 8000
    pure ()
