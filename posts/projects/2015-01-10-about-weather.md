---
author:         Алексей Пирогов
title:          О погоде функционально
tags:           weather,CLI
description:    Написание CLI-утилиты, запрашивающей прогноз погоды у Yahoo
hrefToOriginal: https://astynax.github.io/posts/o-pogode-funktsionalno.html
---

Недавно, очередной раз взглянув за окно, вдруг подумал: *"Интересно, а сколько там градусов за окном?"*. Сегодня, имея браузер под рукой, ответить на этот вопрос несложно, и даже точность прогноза будет вполне приличной. Однако даже этот способ имеет недостаток - **слишком много телодвижений**! И конечно же вожжа попала под нужное место: я решил написать простенькую программку, запрашивающую погоду. Ну и, как водится, на [**Haskell**](https://www.haskell.org/haskellwiki/Haskell)

Выводить погоду я решил в статусную строку моего оконного менеджера ([i3wm](http://i3wm.org/)), а информация, выводимая на оной, должна быть исключительно текстовой. Для краткого прогноза в стиле ```-5, облачно``` вполне подходит. Напросились следующие требования к программе:

* [CLI](https://en.wikipedia.org/wiki/Command-line_interface)-интерфейс
* вывод погоды как для заданного города (на момент написания, это была Казань), так и для любого другого - указанием через опции командной строки
* получение данных о погоде от [Yahoo weather](https://developer.yahoo.com/weather/) (выбор пал на этот сервис, т.к. этот API был уже знаком)
* возможность добавлять в дальнейшем дополнительные сервисы, предоставляющие данные о погоде

Надо сказать, ещё одной целью написания этой программы, помимо, собственно, полезности конечного результата, стала возможность написать что-то практичное на Haskell. А заодно и изучить, как на Haskell

* **запрашивать** что-то **через HTTP**
* **парсить XML**
* **реализовывать CLI-опции**

В таком порядке я реализовал функции приложения, буду придерживаться этого порядка и здесь. Но с начала

## "Типа, типы"

Haskell учит: **Сначала типы, постом всё остальное!**

Посему:

```haskell
import Data.Text

-- Единицы измерения температуры
data TempUnits = Celsiuses
               | Farenheits

data Weather = Weather
             { getCity    :: Text
             , getCountry :: Text
             , getTemp    :: Text
             , getUnits   :: TempUnits
             , getDate    :: Text
             , getText    :: Text }

-- пара алиасов для красоты
type CityID = String
type Url = String
```

Данные о погоде хранятся преимущественно в полях типа ```Text```, т.к. могут содержат символы Unicode, да и получаться будут из XML, текст в котором хранится именно в ```Text```.

Т.к. погоду предстояло выводить в текстовом виде, реализовал приведение ```Weather``` к строке. Вот как это выглядит:

```haskell
renderWeather :: Weather -> String
renderWeather w =
  concat [ unpack (getDate w), ": "
         , unpack (getCity w)
         , "(", unpack (getCountry w), "), "
         , unpack (getTemp w)
         , case getUnits w of
              Celsiuses  -> "°C"
              Farenheits -> "°F"
         , ", "
         , unpack (getText w)]
```

Вывод же получается следующим

```Fri, 21 Nov 2014 9:59 pm MSK: Kazan'(Russia), -5°C, Fog```

> Правда, в конце концов я решил, что выглядит такая "портянка" громоздко и, путем комментирования первых трёх строк, сократил выводимый текст до ```-5°C, Fog```

Ну вот, данные есть, теперь можно вернуться к первоначальному плану. Итак

## "Ну и запросы у вас!"

Работа с протоколом HTTP в Haskell обычно делается силами пакета... HTTP! Кхм, даже скучно как-то. Первый вариант функции отправки запроса с возвратом тела ответа выглядел так:

```haskell
-- Импорты
import Network.HTTP ( catchIO
                    , getRequest, getResponseBody
                    , simpleHTTP )

...

-- собственно, функция отправки запросов
request :: Url -> IO (Maybe String)
request url =
  catchIO (liftM Just (simpleHTTP (getRequest url)
                   >>= getResponseBody))
          (const (return Nothing))
```

Здесь всё довольно просто: результатом неудавшегося запросы будет просто ```Nothing```, а в случае удачи вернётся ```Just "..."``` с телом ответа.

Url же формировать было поручено этой функции:

```haskell
mkAPIUrl :: CityID -> TempUnits -> Url
mkAPIUrl city units =
  let unitStr = case units of
                  Celsiuses  -> "c"
                  Farenheits -> "f"
  in "http://weather.yahooapis.com/forecastrss?w="
  ++ city ++ "&u=" ++ unitStr
```

Здесь тоже всё прозрачно, я считаю.

Этот вариант HTTP-клиента работал у меня отлично дома, однако через офисный **proxy-сервер** пробиться сходу он, увы, не смог. Ну да ладно, на то он и ```simpleHTTP``` - ему простительно. Гугление подсказало, что через прокси может ходить ```Network.HTTP.Browser```. Решено было его и использовать. Теперь код запроса погоды выглядит так:

```haskell
-- Новые импорты
import Network.Browser    (browse, request,
                           setOutHandler, setProxy)
import Network.HTTP       (Response (rspBody),
                           catchIO, getRequest)
import Network.HTTP.Proxy (Proxy, fetchProxy, parseProxy)

...

simpleRequest :: Maybe Proxy
              -> Url
              -> IO (Maybe String)
simpleRequest mbProxy url =
  catchIO (liftM Just get)
          (const (return Nothing))
  where
    get :: IO String
    get = do
      -- если proxy не указан явно, программа будет
      -- пытаться получить его настройки от ОС
      p <- maybe (fetchProxy False) return mbProxy
      (_, res) <- browse $ do
        setProxy p
        -- весь вывод в консоль от браузера подавляется
        setOutHandler $ const $ return ()
        request (getRequest url)
      return (rspBody res)
```

## "Что-то неразборчив ваш XML, без линз никак!"

XML-документы в Haskell хранятся в типе ```Document``` из модуля ```Text.XML```. Получить же документ из строки с содержимым можно так:

```haskell
import qualified Data.Text    as T
import           Data.Default (def)
import qualified Text.XML     as X
import           Text.XML     (Document)

...

parseDocument :: String -> Maybe Document
parseDocument s =
  case X.parseText def (T.pack s) of
    Right d -> Just d
    _       -> Nothing
```

```parseText``` возвращает ```Either```, содержащий описание ошибок парсинга. Однако, данная задача не требует таких подробностей, поэтому функция, приведенная выше, возвращает просто ```Maybe Document```.

Документ получать уже можно, но ведь нужно ещё и работать с ним. Будучи знаком с линзами, я прямо таки чувствовал, что XML можно обрабатывать и с их помощью. Так и вышло: нашелся пакет ```xml-lens```! Разбор документа далее будет производиться с его помощью:

```haskell
import Control.Applicative ((<$>), (<*>))
import Text.XML.Lens       (attr, el, named, root, (./), (^?))

getWeather :: Document -> Maybe Weather
getWeather doc =
  do feed      <- doc ^? root . el "rss" ./ el "channel"
     units     <- feed ^? el "channel" ./ named "units" . attr "temperature"
     city      <- feed ^? el "channel" ./ named "location" . attr "city"
     country   <- feed ^? el "channel" ./ named "location" . attr "country"
     condition <- feed ^? el "channel" ./ el "item" ./ named "condition"
     Weather city country
         <$> condition ^? attr "temp"
         <*> toTempUnit units
         <*> condition ^? attr "date"
         <*> condition ^? attr "text"
  where
    toTempUnit :: Text -> Maybe TempUnits
    toTempUnit = flip lookup [("C", Celsiuses), ("F", Farenheits)]
```

XML-линзы выглядят необычно, но для тем, кто знаком с линзами в целом, разобраться труда не составит. Сам код же вполне читаем получился, что особо порадовало.

## "Также доступны следующие опции..."

Опции программы нужно где-то хранить, для этой цели был добавлен тип:

```haskell
data Config = Config
            { cityID    :: Maybe CityID
            , tempUnits :: TempUnits
            , proxy     :: Maybe Proxy }
```

Командную строку я решил разбирать с помощью библиотеки ```optparse-applicative```. Встретил её я какое то время назад, но использовать пакет ещё не доводилось. Здесь же библиотека очень пригодилась. Парсер опций с выглядит так:

```haskell
import Options.Applicative       (Parser, execParser, flag, fullDesc,
                                  header, help, helper, info, long,
                                  metavar, option, progDesc, short,
                                  strOption, (<>))
import Options.Applicative.Types (ParseError (ErrorMsg), ReadM (..))

...

opts :: Parser Config
opts = Config
  <$> optional (strOption
      (long "city"
    <> short 'c'
    <> metavar "CITY"
    <> help "Yahoo weather API's city ID"))

  <*> flag Celsiuses Farenheits
      (long "farenheits"
    <> short 'F'
    <> help "Show temperature in Farenheits (default: Celsiuses)")

  <*> optional (option extractProxy
      (long "proxy"
    <> short 'p'
    <> help "Proxy server in format [user:pass@]host[:port]"))

  where
    extractProxy =
      ReadM . maybe (Left (ErrorMsg "Wrong proxy string! (see --help)")) Right
            . parseProxy
```

Сам же интерфейс командной строки описывается так:

```haskell
cli :: IO Config
cli = execParser
    $ info (helper <*> opts)
      (fullDesc
    <> progDesc "Print current weather for CITY"
    <> header "weather - Yahoo Weather displaying tool")
```

Библиотека, кроме собственно реализации опций, даёт ещё и возможность генерировать автоматически справку по ключам и опциям. Выглядит справка так:

    $ weather --help
    weather - Yahoo Weather displaying tool
    
    Usage: weather [-c|--city CITY] [-F|--farenheits] [-p|--proxy ARG]
      Print current weather for CITY

    Available options:
      -h,--help                Show this help text
      -c,--city CITY           Yahoo weather API's city ID
      -F,--farenheits          Show temperature in Farenheits (default: Celsiuses)
      -p,--proxy ARG           Proxy server in format [user:pass@]host[:port]


## "Всех их вместе соберем!"

Ну вот, в общем то, и всё, что нужно для сборки готовой программы. Осталась самая малость - main-функция:

```haskell
import System.Exit (ExitCode(..), exitWith)

...

main :: IO ()
main = cli >>= doSomeWork >>= exitWith

doSomeWork :: Config -> IO ExitCode
doSomeWork cfg = do
  resp <- simpleRequest
    (proxy cfg)
    (mkAPIUrl (fromMaybe "2121267" -- default city is Kazan'
                         (cityID cfg))
              (tempUnits cfg))
  let weather = resp >>= parseDocument >>= getWeather
  maybe (return $ ExitFailure 1)
        ((>> return ExitSuccess) . putStrLn . renderWeather)
        weather
```

**Всё!**

Вот и готово полезное приложение, и, что ещё важнее, опробованы удобные и мощные инструменты! Целиком же код можно посмотреть [тут](https://bitbucket.org/astynax/weather/overview).

