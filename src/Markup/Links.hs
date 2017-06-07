{-# LANGUAGE OverloadedStrings #-}

module Markup.Links (
    linksTemplate
) where

import qualified Data.Text.Lazy                as Text
import           Hakyll.Web.Template           (Template, readTemplate)
import           Prelude                       hiding (div, span)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              (Html, a, div, h1, ul, li, h3, span, (!))
import qualified Text.Blaze.Html5.Attributes   as A

linksTemplate :: Template
linksTemplate = readTemplate . Text.unpack . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! A.class_ "links" $ do
        official
        tools
        packages
        books
        podcasts
        community
  where
    official :: Html
    official = do
        h3 $ "Язык"
        div $ ul $ do
            li $ a ! A.href "https://www.haskell.org/" $ "Официальный сайт языка Haskell."
            li $ do
                a ! A.href "https://haskell-lang.org/" $ "Альтернативный сайт языка haskell."
                span " Не спрашивайте, почему сайта два, так уж случилось."
            li $ a ! A.href "https://www.haskell.org/onlinereport/haskell2010/" $ "Официальный стандарт Haskell 2010."

    tools :: Html
    tools = do
        h3 $ "Инструменты"
        div $ ul $ do
            li $ do
                a ! A.href "https://www.haskellstack.org/" $ "The Haskell Tool Stack."
                span " Программа для полноценной работы с Haskell-проектами."
            li $ do
                a ! A.href "https://www.haskell.org/platform/" $ "Haskell Platform."
                span " В каком-то смысле слова - альтернатива Stack."
            li $ do
                a ! A.href "http://haskellformac.com/" $ "Haskell for Mac."
                span " Программа для изучения Haskell на macOS. Отличается интерактивными возможностями."

    packages :: Html
    packages = do
        h3 $ "Пакеты"
        div $ ul $ do
            li $ do
                a ! A.href "http://hackage.haskell.org/packages/" $ "Hackage."
                span " Много-премного Haskell-пакетов."
            li $ do
                a ! A.href "https://www.stackage.org/" $ "Stackage."
                span " Стабильные снимки Haskell-пакетов. "
                a ! A.href "https://github.com/fpco/stackage#frequently-asked-questions" $ "Пояснения."
            li $ do
                a ! A.href "https://hayoo.fh-wedel.de/" $ "Hayoo!"
                span " Поиск по Haskell-пакетам номер раз."
            li $ do
                a ! A.href "http://hoogle.haskell.org/" $ "Hoogle."
                span " Поиск по Haskell-пакетам номер два."

    books :: Html
    books = do
        h3 $ "Книги"
        div $ ul $ do
            li $ do
                a ! A.href "https://www.ohaskell.guide/" $ "О Haskell по-человечески."
                span " Для новичков, ничего не знающих про Haskell."
            li $ do
                a ! A.href "https://anton-k.github.io/ru-haskell-book/book/home.html" $ "Учебник по Haskell."
                span " Более основательное введение, больше теории."
            li $ do
                a ! A.href "http://dmkpress.com/catalog/computer/programming/functional/978-5-97060-025-2/" $ "Изучай Haskell во имя добра!"
                span " Знаменитая книга для новичков с весёлыми картинками."

    podcasts :: Html
    podcasts = do
        h3 $ "Подкасты"
        div $ ul $ do
            li $ do
                a ! A.href "https://bananasandlenses.net/" $ "Бананы и Линзы."
                span " На русском языке."
            li $ do
                a ! A.href "http://www.haskellcast.com/" $ "The Haskell Cast."
                span " На английском языке."

    community :: Html
    community = do
        h3 $ "Сообщества"
        div $ ul $ do
            li $ do
                a ! A.href "https://www.reddit.com/r/haskell/" $ "Haskell Reddit."
                span " Основной англоязычный Reddit-канал."
            li $ do
                a ! A.href "https://mail.haskell.org/mailman/listinfo" $ "Haskell Mailing Lists."
                span " Все списки рассылки, преимущественно англоязычные."
            li $ do
                a ! A.href "https://twitter.com/haskellweekly" $ "Haskell Weekly."
                span " Твиттер-аккаунт для еженедельных Haskell-новостей."
