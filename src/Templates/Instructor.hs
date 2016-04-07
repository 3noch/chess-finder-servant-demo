{-# LANGUAGE ExtendedDefaultRules #-}

module Templates.Instructor where

import Lucid


instructorPage :: Int -> Html ()
instructorPage int = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]

    title_ "Instructor Info"

    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.0.17/css/bulma.min.css"]

  body_ $
    section_ [class_ "section"] $
      div_ [class_ "container"] $
        div_ [class_ "heading"] $ do
          h1_ [class_ "title"] "Instructor Info"
          h2_ [class_ "subtitle"] "Instructor " >> toHtml (show int)
          p_ "I don't know anything about him."
