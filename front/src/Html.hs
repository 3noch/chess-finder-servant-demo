module Html where

import Data.Map (Map)
import Reflex (Dynamic, Event, forDyn)
import Reflex.Dom (EventName(Click), MonadWidget, domEvent, el, elAttr, elAttr', elDynAttr)

import ChessFinder.Common


h1_, h2_, p_, strong_, section_, container, table_, th_, tr_, td_ :: MonadWidget t m => m a -> m a
h1_ = elAttr "h1" ["class".="title"]
h2_ = elAttr "h2" ["class".="subtitle"]
p_ = el "p"
strong_ = el "strong"
section_ = elAttr "section" ["class".="section"]
container = elAttr "div" ["class".="container"]
table_ = elAttr "table" ["class".="table is-striped"]
th_ = el "th"
tr_ = el "tr"
td_ = el "td"

divv, section, tr, td :: MonadWidget t m => Map String String -> m a -> m a
divv = elAttr "div"
section = elAttr "section"
tr = elAttr "tr"
td = elAttr "td"


link :: MonadWidget t m => Map String String -> m () -> m (Event t ())
link attrs body = do
  (l, _) <- elAttr' "a" attrs body
  pure $ domEvent Click l

faIcon :: MonadWidget t m => String -> m ()
faIcon icon = elAttr "span" ["class".="icon"] $ elAttr "i" ["class".="fa fa-" ++ icon] (pure ())

faIconDyn :: MonadWidget t m => Dynamic t String -> m ()
faIconDyn iconDyn = do
  attrDyn <- iconDyn `forDyn` \icon -> ["class".="fa fa-" ++ icon]
  elAttr "span" ["class".="icon"] $ elDynAttr "i" attrDyn (pure ())

br_, loading :: MonadWidget t m => m ()
br_ = el "br" (pure ())
loading = elAttr "a" ["class".="button is-loading"] (pure ())
