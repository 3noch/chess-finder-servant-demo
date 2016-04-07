module Auth where

import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))
import Servant.Server
  ( BasicAuthCheck(BasicAuthCheck)
  , BasicAuthResult(Authorized, Unauthorized)
  , Context((:.), EmptyContext)
  )

authCheck :: BasicAuthCheck Bool
authCheck = BasicAuthCheck check
  where
    check (BasicAuthData username password) = do
      putStrLn "Checking auth"
      if username == "servant" && password == "server"
        then putStrLn "Good!" >> pure (Authorized True)
        else putStrLn "Bad" >> pure Unauthorized


basicAuthServerContext :: Context (BasicAuthCheck Bool ': '[])
basicAuthServerContext = authCheck :. EmptyContext
