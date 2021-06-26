{-# LANGUAGE OverloadedStrings #-}

module Markup.About (
    aboutTemplate
) where
import           Control.Monad ((<=<))
import qualified Data.Text.Lazy as Text
import           Hakyll (Compiler, makeItem)
import           Hakyll.Web.Template (Template, compileTemplateItem)
import           Prelude hiding (div, span)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 (Html, a, br, div, h1, span, (!))
import qualified Text.Blaze.Html5.Attributes as A

aboutTemplate :: Compiler Template
aboutTemplate = compileTemplateItem <=< makeItem . Text.unpack . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "about-us" $ do
        weAre
        contactUs
        ourFriends
  where
    weAre :: Html
    weAre = "Мы — сообщество русскоговорящих Haskell-разработчиков. Основанное в 2014 году группой единомышленников, сообщество объединяет как опытных хаскелистов, так и тех, кто только заинтересовался этим языком. Мы способствуем продвижению Haskell и связанных с ним технологий как в России, так и за её пределами. Наше желание — не просто говорить об этом прекрасном языке, но использовать его в нашей повседневной работе."

    contactUs :: Html
    contactUs =
        div $ do
            br
            "Если у вас есть вопросы, пожелания или предложения — напишите " :: Html
            a ! A.href "mailto:me@dshevchenko.biz" $ "Денису Шевченко"
            " или " :: Html
            a ! A.href "mailto:cblp@cblp.su" $ "Юрию Сыровецкому"
            "." :: Html

    ourFriends :: Html
    ourFriends =
        div $ do
            br
            "Наши друзья:" :: Html
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
