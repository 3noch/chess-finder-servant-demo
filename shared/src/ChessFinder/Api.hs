module ChessFinder.Api where

import Lucid (Html)
import Servant.API (BasicAuth, Capture, Get, JSON, QueryParam, (:>), (:<|>))
import Servant.HTML.Lucid (HTML)

import ChessFinder.Types (Geocontext, Listing)


type Api = "listings"
         :> BasicAuth "chess-finder" Bool
         :> QueryParam "lat" Double
         :> QueryParam "long" Double
         :> Get '[JSON] [Geocontext Listing]

type ServerApi = Api
            :<|> "instructor"
              :> Capture "id" Int
              :> Get '[HTML] (Html ())
