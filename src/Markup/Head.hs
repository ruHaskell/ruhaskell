{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Markup.Head (
    commonHead
) where

import           Css.Own (ownCss)
import           Misc (aHost)

import           Prelude hiding (div, head, span)

import           Data.Monoid
import           Data.String.QQ
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

commonHead :: Html
commonHead = head $ do
    title "$title$"

    meta ! A.charset "utf-8"

    -- Для мобильных экранов.
    meta ! A.name "viewport"
         ! A.content "width=device-width, initial-scale=1.0"

    meta ! A.name "description"
         ! A.content ""

    meta ! A.name "author"
         ! A.content ""

    link ! A.rel "icon"
         ! A.type_ "image/png"
         ! A.href (toValue $ imgRoot <> "favicon.ico")

    cssLink "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    jsLink  "https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"
    cssLink "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/css/bootstrap.min.css"
    jsLink  "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/js/bootstrap.min.js"
    jsLink  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

    style $ toHtml ownCss

    -- Аналитика.
    script $ toHtml googleAnalytics

    script $ toHtml ("window.___gcfg = { lang: 'ru' };" :: String)
    script ! A.src "https://apis.google.com/js/platform.js" $ "" -- async defer

    -- RSS feed.
    link ! A.rel "alternate"
         ! A.type_ "application/rss+xml"
         ! A.title "RSS"
         ! A.href (toValue $ aHost ++ "/feed.xml")
  where
    cssLink :: String -> Html
    cssLink url = link ! A.rel "stylesheet" ! A.href (toValue url)

    jsLink :: String -> Html
    jsLink url = script ! A.src (toValue url) $ ""

    imgRoot :: String
    imgRoot = "/static/images/"

googleAnalytics :: String
googleAnalytics = [s|
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-58217738-1', 'auto');
ga('send', 'pageview');
|]
