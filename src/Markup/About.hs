{-# LANGUAGE OverloadedStrings #-}

module Markup.About (
    aboutTemplate
) where

import           Prelude                            hiding ( div, span )
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.Blaze.Html.Renderer.Pretty    ( renderHtml )
import           Hakyll.Web.Template
import           Data.Text                          ( Text )

aboutTemplate :: Template
aboutTemplate = readTemplate . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "about-us" $ do
        toHtml weAre
        contactUs
        ourFriends
  where
    weAre :: Text
    weAre = "Мы — сообщество русскоговорящих Haskell-разработчиков. Основанное в 2014 году группой единомышленников, сообщество объединяет как опытных хаскелистов, так и тех, кто только заинтересовался этим языком. Мы способствуем продвижению Haskell и связанных с ним технологий как в России, так и за её пределами. Наше желание — не просто говорить об этом прекрасном языке, но использовать его в нашей повседневной работе."
    
    contactUs :: Html
    contactUs =
        div $ do
            preEscapedToHtml ("<br/>" :: String)
            span "Если у вас есть вопросы, пожелания или предложения — напишите"
            a ! A.href "mailto:me@dshevchenko.biz" $ "Денису Шевченко"
            span " или "
            a ! A.href "mailto:cblp@cblp.su" $ "Юрию Сыровецкому"
            span "."

    ourFriends :: Html
    ourFriends =
        div $ do
            preEscapedToHtml ("<br/>" :: String)
            span "Наши друзья:"
            span ! A.class_ "friends-separator" $ "" 
            a ! A.href "http://fpconf.ru/"
              ! A.id "fpconf-link"
              ! A.class_ "fpconf-link"
              ! A.title "Конференция о функциональном программировании в России" $ "#FPCONF"
            span ! A.class_ "friends-separator" $ "" 
            a ! A.href "http://fby.by/" 
              ! A.id "fby-link"
              ! A.class_ "fby-link"
              ! A.title "Конференция по функциональному программированию в Беларуси" $ "#FUNCBY"
