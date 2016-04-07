module Handlers where

import MyPrelude
import qualified Data.Map.Strict as Map
import Network.HTTP.Client (Manager)
import Servant (ServantErr, err403)
import Servant.API ((:<|>)(..))
import Servant.Server
  (Server, ServerT
  , (:~>)(Nat)
  , enter
  , err504
  )

import ChessFinder.Api (ServerApi)
import ChessFinder.Types
  ( Geocontext(Geocontext)
  , LatLong(LatLong)
  , Listing(Listing, listingLocation)
  , Location(Address, Coords)
  )
import GoogleDistanceMatrix
  ( MatrixElement(elem_duration)
  , MatrixCell(cell_text)
  , getDistanceMatrix
  )
import Templates.Instructor (instructorPage)


type App = ReaderT Manager (ExceptT ServantErr IO)

runAppT :: Manager -> App a -> ExceptT ServantErr IO a
runAppT httpClientManager action = runReaderT action httpClientManager

handlersServer :: Manager -> Server ServerApi
handlersServer httpClientManager = enter (Nat $ runAppT httpClientManager) handlers

handlers :: ServerT ServerApi App
handlers = getListings :<|> return . instructorPage


getListings :: Bool -> Maybe Double -> Maybe Double -> App [Geocontext Listing]
getListings True (Just lat) (Just long) = do
  let origin = Coords $ LatLong lat long

  listingMap <- pure $ Map.fromList [(origin, listingLocation x) .= x | x <- fakeListings]

  httpClientManager <- ask
  response <- liftIO $ runExceptT $
    getDistanceMatrix httpClientManager [origin] (listingLocation <$> fakeListings)

  case response of
    Left _ -> throwError err504
    Right distanceMap -> do
      let combine listing distance = Geocontext listing (cell_text . elem_duration $ distance)

      return $ Map.elems $ Map.intersectionWith combine listingMap distanceMap

getListings True _ _  = return [Geocontext listing "?" | listing <- fakeListings]
getListings False _ _ = throwError err403

home :: Location
home = Address "Louisville, KY"

fakeListings :: [Listing]
fakeListings =
  [ Listing "Become a Master" "Roy Almasy" 200.0 home
  , Listing "Learn the Basic Rules" "Elliot Cameron" 300.0 (Address "Jeffersonville, IN")
  , Listing "Imagine Your Own World" "Oprah" 500.0 (Address "Lexington, KY")
  , Listing "YUMmy Chess" "Colonel Sanders" 220.0 (Address "1 W Main St, Louisville, KY")
  , Listing "Chester Peak" "Henry Wollop" 150.0 (Address "New York, NY")
  ]
