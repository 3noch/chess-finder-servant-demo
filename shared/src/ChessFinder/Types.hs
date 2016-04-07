module ChessFinder.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Data.Text (Text)
import GHC.Generics (Generic, Rep)


data Listing = Listing
  { listingName           :: !String
  , listingInstructorName :: !String
  , listingCost           :: !Double
  , listingLocation       :: !Location
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Listing
instance ToJSON   Listing


data Geocontext a = Geocontext { unGeocontext :: a, geodistanceTime :: !Text }

deriving instance Eq      a => Eq      (Geocontext a)
deriving instance Generic a => Generic (Geocontext a)
deriving instance Ord     a => Ord     (Geocontext a)
deriving instance Show    a => Show    (Geocontext a)
instance (FromJSON a, Generic a) => FromJSON (Geocontext a)
instance (ToJSON   a, Generic a) => ToJSON   (Geocontext a)


data Location = Address !Text | Coords !LatLong
              deriving (Eq, Generic, Show, Ord)

instance FromJSON Location
instance ToJSON   Location


data LatLong = LatLong { latitude :: !Double, longitude :: !Double }
             deriving (Eq, Generic, Ord, Show)

instance FromJSON LatLong
instance ToJSON   LatLong



parseJsonRenamed :: (Generic a, Json.GFromJSON (Rep a)) => Json.Value -> Json.Parser a
parseJsonRenamed = Json.genericParseJSON Json.defaultOptions
  { Json.fieldLabelModifier = tail . dropWhile (/= '_') }
