module Main where

import MyPrelude
import Servant.API ((:<|>)(..), Raw)
import Servant.Server (Server, serveWithContext)
import Servant.Utils.StaticFiles (serveDirectory)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)

import ChessFinder.Api (ServerApi)

import Auth (basicAuthServerContext)
--import GoogleDistanceMatrix (getLocations)
import Handlers (handlersServer)

type RealApi = ServerApi :<|> Raw

server :: Manager -> Server RealApi
server httpClientManager =
  handlersServer httpClientManager :<|> serveDirectory "front/_dist"

main :: IO ()
main = do
  httpClientManager <- newManager tlsManagerSettings
  run 8001 $ serveWithContext
    (Proxy :: Proxy RealApi)
    basicAuthServerContext
    (server httpClientManager)
