{-# LANGUAGE OverloadedStrings #-}

module Markup.Footer where

import           Prelude hiding (div, span)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

commonFooter :: Html
commonFooter =
    footer ! A.class_ "footer" $
        div ! A.class_ "container-fluid" $
            div ! A.class_ "row" $ do
                div ! A.class_ "col-lg-4 col-md-4 col-sm-4 col-xs-4 left" $   copyright
                div ! A.class_ "col-lg-4 col-md-4 col-sm-4 col-xs-4 center" $ hakyll
                div ! A.class_ "col-lg-4 col-md-4 col-sm-4 col-xs-4 right" $  mit
  where
    copyright = "© 2014-2018 ruHaskell"

    mit =
        a ! A.href "https://github.com/ruHaskell/ruhaskell/blob/master/LICENSE" ! A.id "mit-link" $ do
            i ! A.class_ "fa fa-balance-scale" ! customAttribute "aria-hidden" "true" $ ""
            span "MIT"

    hakyll =
        a ! A.href "http://jaspervdj.be/hakyll/" ! A.id "hakyll-link" $ do
            span "Мы"
            i ! A.class_ "fa fa-heart heart-color" ! customAttribute "aria-hidden" "true" $ ""
            span "Hakyll"
