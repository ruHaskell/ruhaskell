{-
    Модуль разметки скелета страницы.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Markup.Categories where

import           Prelude hiding (div)

import           Hakyll.Web.Template
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import           Hakyll                        (makeItem, Compiler)
import           Control.Monad                 ( (<=<) )

categoriesTemplate ::Compiler Template
categoriesTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "tags-cloud" $
        "$categoriesCloud$"
