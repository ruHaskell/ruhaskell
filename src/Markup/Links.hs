{-# LANGUAGE OverloadedStrings #-}

module Markup.Links (
    linksTemplate
) where

import           Prelude hiding (div, id)

import qualified Data.Text.Lazy as Text
import           Hakyll.Web.Template (Template, readTemplate)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 (Html, a, div, h1, h3, li, ul, (!))
import           Text.Blaze.Html5.Attributes (class_, href, id)

linksTemplate :: Template
linksTemplate = readTemplate . Text.unpack . renderHtml $ raw

raw :: Html
raw = do
    h1 "$title$"
    div ! class_ "links" $ do
        official
        tools
        packages
        books
        courses
        podcasts
        discussion
  where
    official = do
        h3 "Язык"
        ul $ do
            li $
                a ! href "https://www.haskell.org/" $
                "Официальный сайт языка Haskell."
            li $ do
                a ! href "https://haskell-lang.org/" $
                    "Альтернативный сайт языка haskell."
                " Не спрашивайте, почему сайта два, так уж случилось."
            li $
                a ! href "https://www.haskell.org/onlinereport/haskell2010/" $
                "Официальный стандарт Haskell 2010."

    tools = do
        h3 "Инструменты"
        ul $ do
            li $ do
                a ! href "https://www.haskellstack.org/" $
                    "The Haskell Tool Stack."
                " Программа для полноценной работы с Haskell-проектами."
            li $ do
                a ! href "https://www.haskell.org/platform/" $
                    "Haskell Platform."
                " В каком-то смысле слова - альтернатива Stack."
            li $ do
                a ! href "http://haskellformac.com/" $
                    "Haskell for Mac."
                " Программа для изучения Haskell на macOS."
                " Отличается интерактивными возможностями."

    packages = do
        h3 "Пакеты"
        ul $ do
            li $ do
                a ! href "http://hackage.haskell.org/packages/" $ "Hackage."
                " Много-премного Haskell-пакетов."
            li $ do
                a ! href "https://www.stackage.org/" $ "Stackage."
                " Стабильные снимки Haskell-пакетов. "
                a   ! href  "https://github.com/fpco/stackage\
                            \#frequently-asked-questions"
                    $ "Пояснения."
            li $ do
                a ! href "https://hayoo.fh-wedel.de/" $ "Hayoo!"
                " Поиск по Haskell-пакетам номер раз."
            li $ do
                a ! href "http://hoogle.haskell.org/" $ "Hoogle."
                " Поиск по Haskell-пакетам номер два."

    books = do
        h3 "Книги"
        ul $ do
            li $ do
                a ! href "https://www.ohaskell.guide/" $
                    "О Haskell по-человечески."
                " Для новичков, ничего не знающих про Haskell."
            li $ do
                a   ! href  "https://anton-k.github.io\
                            \/ru-haskell-book/book/home.html"
                    $ "Учебник по Haskell."
                " Более основательное введение, больше теории."
            li $ do
                a   ! href  "http://dmkpress.com\
                            \/catalog/computer/programming/functional\
                            \/978-5-97060-025-2/"
                    $ "Изучай Haskell во имя добра!"
                " Знаменитая книга для новичков с весёлыми картинками."

    courses = do
        h3 "Курсы"
        ul $
            li $ do
                "Курс Дениса Москвина на Степике: "
                a ! href "https://stepik.org/course/75/" $ "часть 1"
                ", "
                a ! href "https://stepik.org/course/693/" $ "часть 2"

    podcasts = do
        h3 "Подкасты"
        ul $ do
            li $ do
                a ! href "https://bananasandlenses.net/" $ "Бананы и Линзы."
                " На русском языке."
            li $ do
                a ! href "http://www.haskellcast.com/" $ "The Haskell Cast."
                " На английском языке."

    discussion =
        div ! id "discussion" $ do
            h3 "Общение"
            ul $ do
                li $ do
                    a ! href "https://www.reddit.com/r/haskell/" $ "Haskell Reddit."
                    " Основной англоязычный Reddit-канал."
                li $ do
                    a ! href "https://mail.haskell.org/mailman/listinfo" $
                        "Haskell Mailing Lists."
                    " Все списки рассылки, преимущественно англоязычные."
                li $ do
                    a ! href "https://twitter.com/haskellweekly" $ "Haskell Weekly."
                    " Твиттер-аккаунт для еженедельных Haskell-новостей."
