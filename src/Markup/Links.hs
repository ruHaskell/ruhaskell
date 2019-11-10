{-# LANGUAGE OverloadedStrings #-}

module Markup.Links (
    linksTemplate
) where

import           Prelude hiding (div, id)

import qualified Data.Text.Lazy as Text
import           Hakyll.Web.Template (Template, readTemplate)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5 (Html, a, div, h1, h3, li, strong, ul, (!))
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
            li $ do
                a ! href "https://www.stackage.org/lts/hoogle" $
                    "Stackage Hoogle."
                " Поиск по Haskell-пакетам номер три."

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
            li $ do
                a ! href "https://typeclasses.com/phrasebook" $
                    "The Haskell Phrasebook"
                " — хаскельный разговорник"

    courses = do
        h3 "Курсы"
        ul $ do
            li $ do
                "Курс Дениса Москвина на Степике: "
                a ! href "https://stepik.org/course/75/" $ "часть 1"
                ", "
                a ! href "https://stepik.org/course/693/" $ "часть 2"
            li $ do
                a ! href "https://www.edx.org/course/\
                         \introduction-functional-programming-delftx-fp101x-0" $
                    "Introduction to Functional Programming"
                " — вводный курс по ФП от Эрика Мейера с примерами на Haskell"
            li $ do
                a ! href "https://www.intuit.ru/studies/courses/3652/894/info" $
                    "Haskell как первый язык программирования"
                " от "
                a   ! href
                        "https://ru.wikipedia.org/wiki/\
                        \Абрамов,_Сергей_Михайлович_(учёный)"
                    $ "Сергея Михайловича Абрамова"

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
                    strong "Gitter"
                    ul $ do
                        li $
                            a ! href "https://gitter.im/ruHaskell/home" $
                            strong "Список чатов"
                        li $ do
                            a ! href "https://gitter.im/ruHaskell/forall" $
                                "forall"
                            " — основной чат"
                        li $ do
                            a ! href "https://gitter.im/ruHaskell/meetup" $
                                "meetup"
                            " — встречи сообщества"
                        li $ do
                            a ! href "https://gitter.im/ruHaskell/novice" $
                                "novice"
                            " — чат для новичков"
                        li $ do
                            a ! href "https://gitter.im/ruHaskell/jobs" $
                                "jobs"
                            " — вакансии и резюме"
                        li $ do
                            a ! href "https://gitter.im/ruHaskell/blah" $
                                "blah"
                            " — оффтопик"
                li $ do
                    strong "Reddit"
                    ul $ do
                        li $ do
                            a ! href "https://www.reddit.com/r/haskell" $
                                "/r/haskell"
                            " — основной англоязычный субреддит"
                        li $ do
                            a ! href "https://www.reddit.com/r/ruhaskell" $
                                "/r/ruhaskell"
                            " — русскоязычный субреддит"
                li $ do
                    strong "Telegram"
                    ul $ do
                        li $ do
                            a ! href "https://t.me/haskellru" $ "@haskellru"
                            " — основной хаскельный чат"
                        li $ do
                            a ! href "https://t.me/haskell_learn" $
                                "@haskell_learn"
                            " — чат для новичков"
                        li $ do
                            a ! href "https://t.me/haskell_job" $ "@haskell_job"
                            " — канал с хаскельными вакансиями"
                        li $ do
                            a ! href "https://t.me/haskell_cv" $ "@haskell_cv"
                            " — чат для поиска работы"
                            " и обсуждения работодателей"
                        li $ do
                            a ! href "https://t.me/haskell_en" $ "@haskell_en"
                            " — англоязычный хаскельный чат"
                        li $ do
                            a ! href "https://t.me/haskell_blah" $
                                "@haskell_blah"
                            " — оффтопик"
                li $ do
                    strong "Twitter"
                    ul $ do
                        li $ do
                            a ! href "https://twitter.com/haskellweekly" $
                                "Haskell Weekly"
                            " — еженедельные хаскельные новости"
                        li $ do
                            a ! href "https://twitter.com/ruhaskell" $
                                "ruHaskell"
                            " — новости русскоязычного сообщества"
                li $ do
                    a ! href "https://mail.haskell.org/mailman/listinfo" $
                        "Haskell Mailing Lists"
                    " — все списки рассылки, преимущественно англоязычные"
