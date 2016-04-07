module Cookies
  ( getCookiesRaw
  , setCookiesRaw
  , getCookies
  , setCookie
  , readCookies
  , saveCookies
  , CookiesText
  ) where

import           Control.Exception (ErrorCall(..), SomeException, catch, throwIO)
import           Control.Monad.IO.Class (liftIO)

import           Blaze.ByteString.Builder (toByteString)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           GHCJS.DOM (currentDocument)
import qualified GHCJS.DOM.Document as Doc (getCookie, setCookie)
import           GHCJS.DOM.Types (Document)
import           Reflex (Event, ffor)
import           Reflex.Dom (MonadWidget)
import           Reflex.Dom.Class (performEvent, performEvent_)
import           Web.Cookie
  ( CookiesText
  , SetCookie
  , parseCookiesText
  , renderCookies
  , renderSetCookie
  )


getCookiesRaw :: IO (Maybe Text)
getCookiesRaw = (Doc.getCookie =<< orBombOut =<< currentDocument)
                `catch` \(e :: SomeException) -> pure Nothing

setCookiesRaw :: Text -> IO ()
setCookiesRaw txt = (currentDocument >>= orBombOut >>= (`Doc.setCookie` Just txt))
                    `catch` \(e :: SomeException) -> pure ()


getCookies :: IO (Maybe CookiesText)
getCookies = fmap (parseCookiesText . encodeUtf8) <$> getCookiesRaw

setCookie :: SetCookie -> IO ()
setCookie = setCookiesRaw . decodeUtf8 . toByteString . renderSetCookie


saveCookies :: MonadWidget t m => Event t [SetCookie] -> m (Event t ())
saveCookies event = performEvent $ event `ffor` \cookies ->
  liftIO $ mapM_ setCookie cookies

readCookies :: MonadWidget t m => Event t a -> m (Event t (Maybe CookiesText))
readCookies event = performEvent $ event `ffor` const (liftIO getCookies)


orBombOut :: Maybe a -> IO a
orBombOut = maybe (throwIO (ErrorCall "No cookies for you")) pure
