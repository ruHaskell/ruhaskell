{-
    Модуль разметки скелета страницы.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015-2016 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Markup.Default (
    defaultTemplate
) where

import           Prelude hiding (div, span)

import           Data.Text (Text)
import           Hakyll.Web.Template (Template, compileTemplateItem)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5 (Html, a, body, customAttribute, div,
                                   docTypeHtml, i, img, preEscapedToHtml, span,
                                   (!))
import qualified Text.Blaze.Html5.Attributes as A

import           Control.Monad ((<=<))
import           Hakyll (Compiler, makeItem)
import           Markup.Footer
import           Markup.Head

defaultTemplate :: Compiler Template
defaultTemplate = compileTemplateItem <=< makeItem  . renderHtml $ raw

raw :: Html
raw = docTypeHtml $ do
    commonHead
    body $ do
        div ! A.class_ "container-fluid" $
            navigation
        div ! A.class_ "container" $
            lastPosts
        commonFooter

navigation :: Html
navigation =
    div ! A.class_ "navigation" $
        div ! A.class_ "row" $ do
            div ! A.class_ "col-lg-5 col-md-5 col-sm-12 col-xs-12" $ links
            div ! A.class_ "col-lg-2 col-md-2 col-sm-12 col-xs-12" $ logoArea
            div ! A.class_ "col-lg-5 col-md-5 col-sm-12 col-xs-12" $ socialLinks

links :: Html
links =
    div ! A.class_ "links" $ do
        a ! A.href "/authors.html" ! A.id "authors-link" $ "Авторы"
        span ! A.class_ "links-separator" $ ""
        a ! A.href "/tags.html" ! A.id "tags-link" $ "Темы"
        span ! A.class_ "links-separator" $ ""
        a ! A.href "/categories.html" ! A.id "categories-link" $ "Разделы"
        span ! A.class_ "links-separator" $ ""
        a ! A.href "/about.html" ! A.id "about-link" $ "О нас"
        span ! A.class_ "links-separator" $ ""
        a ! A.href "/links.html" ! A.id "links-link" $ "Ресурсы"

logoArea :: Html
logoArea =
    div ! A.class_ "logo-area" $
        a ! A.href "/"
          ! A.title "Домой"
          ! A.id "go-home" $
            img ! A.src "/static/images/logo.svg"
                ! A.alt "ruHaskell"
                ! A.width "100"

socialLinks :: Html
socialLinks =
    div ! A.class_ "social-links" $ do
        a ! A.href "/links.html#discussion" ! A.id "sl-1" ! A.title "Общение" $
            i ! A.class_ "fa fa-commenting gitter-color" ! customAttribute "aria-hidden" "true" $ ""

        span ! A.class_ "social-links-separator" $ ""

        a ! A.href "https://twitter.com/ruHaskell" ! A.id "sl-2" ! A.title "Следите за нашим Твиттером" $
            i ! A.class_ "fa fa-twitter twitter-color" ! customAttribute "aria-hidden" "true" $ ""

        span ! A.class_ "social-links-separator" $ ""

        a ! A.href "https://github.com/ruHaskell/ruhaskell" ! A.id "sl-4" ! A.title "Мы живём на GitHub" $
            i ! A.class_ "fa fa-github github-color" ! customAttribute "aria-hidden" "true" $ ""

        span ! A.class_ "social-links-separator" $ ""

        a ! A.href "/feed.xml" ! A.id "sl-5" ! A.title "Наш RSS" $
            i ! A.class_ "fa fa-rss rss-color" ! customAttribute "aria-hidden" "true" $ ""

lastPosts :: Html
lastPosts =
    div ! A.id "content" $ preEscapedToHtml ("$body$" :: Text)
