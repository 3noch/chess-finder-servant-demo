module GoogleDistanceMatrix where

import MyPrelude
import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson as Json
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Servant.API ((:>), Get, JSON, QueryParam)
import Servant.Client (client, ClientM, BaseUrl(BaseUrl), Scheme(Https), ServantError)
import Web.HttpApiData (ToHttpApiData, toUrlPiece)

import ChessFinder.Types
  ( LatLong(LatLong)
  , Location(Address, Coords)
  , parseJsonRenamed
  )


newtype GoogleApiKey = GoogleApiKey Text deriving (Eq, Generic, Ord, Show, ToHttpApiData)

key :: GoogleApiKey
key = GoogleApiKey "[enter-your-google-api-key-here]"

googleBase :: BaseUrl
googleBase = BaseUrl Https "maps.googleapis.com" 443 "/"

type Api = "maps" :> "api" :> "distancematrix" :> "json"
         :> QueryParam "key" GoogleApiKey
         :> QueryParam "origins" [ApiLocation]
         :> QueryParam "destinations" [ApiLocation]
         :> Get '[JSON] MatrixResult


newtype ApiLocation = ApiLocation Location deriving (Eq, Show)

instance ToHttpApiData ApiLocation where
  toUrlPiece (ApiLocation (Address addr))             = toUrlPiece addr
  toUrlPiece (ApiLocation (Coords (LatLong lat lng))) = toUrlPiece lat <> "," <> toUrlPiece lng

instance ToHttpApiData [ApiLocation] where
  toUrlPiece locs = T.intercalate "|" (toUrlPiece <$> locs)

data Status = Ok | InvalidRequest | MaxElementsExceeded | OverQueryLimit
            | RequestDenied | UnknownError
            deriving (Bounded, Enum, Eq, Ord, Show)

instance FromJSON Status where
  parseJSON (Json.String "OK")                    = pure Ok
  parseJSON (Json.String "INVALID_REQUEST")       = pure InvalidRequest
  parseJSON (Json.String "MAX_ELEMENTS_EXCEEDED") = pure MaxElementsExceeded
  parseJSON (Json.String "OVER_QUERY_LIMIT")      = pure OverQueryLimit
  parseJSON (Json.String "REQUEST_DENIED")        = pure RequestDenied
  parseJSON (Json.String "UNKNOWN_ERROR")         = pure UnknownError
  parseJSON _ = mzero

data MatrixResult = MatrixResult
  { result_status :: Status
  , result_rows :: [MatrixRow]
  } deriving (Eq, Generic, Show)

data MatrixRow = MatrixRow
  { row_elements :: [MatrixElement]
  } deriving (Eq, Generic, Show)

data MatrixElement = MatrixElement
  { elem_status   :: Status
  , elem_duration :: MatrixCell
  , elem_distance :: MatrixCell
  } deriving (Eq, Generic, Show)

data MatrixCell = MatrixCell
  { cell_value :: Int
  , cell_text  :: Text
  } deriving (Eq, Generic, Show)


instance FromJSON MatrixResult  where parseJSON = parseJsonRenamed
instance FromJSON MatrixRow     where parseJSON = parseJsonRenamed
instance FromJSON MatrixElement where parseJSON = parseJsonRenamed
instance FromJSON MatrixCell    where parseJSON = parseJsonRenamed


distanceMatrixRaw :: Manager -> Maybe [ApiLocation] -> Maybe [ApiLocation] -> ClientM MatrixResult
distanceMatrixRaw manager froms tos =
  client (Proxy :: Proxy Api) (Just key) froms tos manager googleBase


data DistanceMatrixError = NotAllOk MatrixResult | ApiError ServantError deriving (Show)

getDistanceMatrix :: Manager -> [Location] -> [Location]
                  -> ExceptT DistanceMatrixError IO (Map (Location, Location) MatrixElement)
getDistanceMatrix manager froms tos = do
  -- Get distance matrix result wrapping any error as an ApiError
  matrix <- withExceptT ApiError $
    distanceMatrixRaw manager (Just (ApiLocation <$> froms)) (Just (ApiLocation <$> tos))

  -- Error out if any of the statuses are not OK
  unless (result_status matrix == Ok
          && all (Ok ==) [elem_status el | rows <- result_rows matrix, el <- row_elements rows]) $
    throwError (NotAllOk matrix)

  return $ Map.fromList [ (from, to) .= el
                        | (from, row) <- zip froms (result_rows matrix)
                        , (to, el) <- zip tos (row_elements row)
                        ]
