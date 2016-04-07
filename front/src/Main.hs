{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, OverloadedLists #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
module Main where

import Control.Monad (void)
import Control.Lens ((^.))
import Data.List (sortBy)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (compare)

import Data.String.Conv (toS)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Reflex
import Reflex.Dom hiding (link)
import Reflex.Dom.Contrib.Geoposition
  ( GeopositionInfo(..)
  , attachGeoposition
  , geoLatitude
  , geoLongitude
  )
import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))
import Servant.Reflex (BaseUrl(BaseUrl), Scheme(Http), client)
import Text.Read (readMaybe)
import Web.Cookie (SetCookie, setCookieName, setCookieValue)

import ChessFinder.Api (Api)
import ChessFinder.Common
import ChessFinder.Types
  ( Geocontext(..)
  , LatLong(..)
  , Listing(..)
  , Location(..)
  )

import Cookies
import Html


main :: IO ()
main = do
  cookies <- (fromMaybe [] <$> getCookies)
  let latLong = do
        lat  <- readMaybe . T.unpack =<< lookup "latitude"  cookies
        long <- readMaybe . T.unpack =<< lookup "longitude" cookies
        pure (lat, long)
  mainWidget $ mainHeader latLong

-- Turns an Event with an Either payload into two Events, one for each possible side of
-- the payload. When the original Event fires, only one of the resulting Events will fire.
extractEitherEvent :: Reflex t => Event t (Either a b) -> (Event t a, Event t b)
extractEitherEvent event =
  ( either Just (const Nothing) `fmapMaybe` event
  , either (const Nothing) Just `fmapMaybe` event
  )


clearLatLong :: MonadWidget t m => Event t () -> m (Event t ())
clearLatLong event = do
    saveCookies $ event `ffor` const
      [ def {setCookieName = "latitude",  setCookieValue = ""}
      , def {setCookieName = "longitude", setCookieValue = ""}
      ]

getLatAndLong :: MonadWidget t m => Event t () -> m (Event t (Double, Double))
getLatAndLong event = do
  geoPosEvent <- fmap fst <$> attachGeoposition event
  let (failure, success) = extractEitherEvent geoPosEvent
  saveCookies $ success `ffor` \GeopositionInfo{..} ->
    [ def {setCookieName = "latitude",  setCookieValue = toS (show geoLatitude)}
    , def {setCookieName = "longitude", setCookieValue = toS (show geoLongitude)}
    ]
  pure $ leftmost
    [ failure `ffor` const (0, 0)
    , success `ffor` \GeopositionInfo{..} -> (geoLatitude, geoLongitude)
    ]

mainHeader :: forall t m. MonadWidget t m => Maybe (Double, Double) -> m ()
mainHeader maybeLatLong = do
  let defaultUrl = BaseUrl Http "localhost" 8082 ""
  let
    getListings :: MonadWidget t m
                => Behavior t (Maybe BasicAuthData)
                -> Behavior t (Maybe Double)
                -> Behavior t (Maybe Double)
                -> Event t ()
                -> m (Event t (Maybe [Geocontext Listing], XhrResponse))

    getListings = client
        (Proxy :: Proxy Api)
        (Proxy :: Proxy m)
        (constDyn defaultUrl)

  initEvent <- getPostBuild

  initLatLong <- case maybeLatLong of
    Just latLong -> pure (const latLong <$> initEvent)
    Nothing      -> getLatAndLong initEvent

  rec
      let latLongUpdated = leftmost [initLatLong, resetLatLong]
      latLong <- hold (0, 0) latLongUpdated

      ajaxTrigger <- delay 0 (const () <$> latLongUpdated)

      listingsAjax <- getListings
        (constant $ Just $ BasicAuthData "servant" "server")
        (Just . fst <$> latLong)
        (Just . snd <$> latLong)
        ajaxTrigger
      let dataLoadedEvent = fromMaybe [] . fst <$> listingsAjax

      listings <- holdDyn [] dataLoadedEvent

      reset <- section_ $
        container $ do
          section ["class".="hero is-primary is-medium is-bold"] $ do
            divv ["class".="hero-content"] $
              container $ do
                h1_ $ text "Chess Finder"
                h2_ $ text "find your check mate"
          listingsTable listings
          button "Reset"

      clearCookies <- clearLatLong reset
      resetLatLong <- getLatAndLong clearCookies

  pure ()

dynTextBy :: MonadWidget t m => (a -> String) -> Dynamic t a -> m ()
dynTextBy toStr dyn = do
  str <- toStr `mapDyn` dyn
  dynText str

listingsTable :: MonadWidget t m => Dynamic t [Geocontext Listing] -> m ()
listingsTable listings = do
  let columns = [ "Class"      .= (compare `on` (listingName . unGeocontext))
                , "Instructor" .= (compare `on` (listingInstructorName . unGeocontext))
                , "Cost"       .= (compare `on` (listingCost . unGeocontext))
                , "Distance"   .= (compare `on` geodistanceTime)
                ]

  let columnSortIds = [ key .= (index, comparison)
                      | (index, (key, comparison)) <- enumerate columns ]

  table_ $ do
    sortEvents <- el "thead" $
      tr_ $ do
        th_ $ text ""
        sortEvents <- sequence
          [ const sortId <<$>> th_ (link [] (text key))
          | (key, sortId) <- columnSortIds ]
        th_ $ text ""

        pure sortEvents

    sortProp <- holdDyn (snd $ last columnSortIds) (leftmost sortEvents)
    (sortColumn, sortFunc) <- splitDyn sortProp
    sortedItems <- combineDyn sortBy sortFunc listings
    keyedItems <- combineDyn
      (\column items -> Map.fromList [
        (column, index) .= item
        | (index, item) <- enumerate items
      ])
      sortColumn
      sortedItems

    void $ listWithKey keyedItems $ \key item -> do
      rec moreInfoIcon <- hidden `forDyn` ("chevron-circle-right" `else'` "chevron-circle-down")
          expandLink <- tr_ $ do
            td ["class".="table-icon"] $ faIcon "map-marker"
            td_ $ strong_ $ dynTextBy (listingName . unGeocontext) item
            td_ $ dynTextBy (listingInstructorName . unGeocontext) item
            td_ ( text "$" >> dynTextBy (show . listingCost . unGeocontext) item )
            td_ ( dynTextBy (toS . geodistanceTime) item >> text " away")
            td ["class".="table-icon table-link"] $ link [] $ faIconDyn moreInfoIcon

          hidden <- toggle True expandLink

      moreInfoAttrs <- hidden `forDyn` \d ->
        ["class".="no-hover"] <> if d then ["style".="display: none"] else []

      elDynAttr "tr" moreInfoAttrs $ do
        td_ $ text ""
        td ["colspan".="5", "class".="no-hover"] $ do
          let showAddress (Address addr) = toS addr
              showAddress (Coords _)     = "unknown"

          strong_ (text "Address: ")
          dynTextBy (showAddress . listingLocation . unGeocontext) item
