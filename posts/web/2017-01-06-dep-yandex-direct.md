---
author: Роман Киреев
title:  "Зависимые типы на примере взаимодействия с Яндекс.Директ API"
tags:   зависимые типы, семейства типов, web, servant
description: На примере Яндекс.Директ API в статье показывается, как типизировать такой запрос к API, тип результата которого зависит от содержимого запроса.
---

Здравствуйте.

Зависимые типы ([The view from the left](http://strictlypositive.org/vfl.pdf), [What is a brief but complete explanation of a pure/dependent type system?](http://cs.stackexchange.com/questions/49888/what-is-a-brief-but-complete-explanation-of-a-pure-dependent-type-system)) набирают популярность и в будущем [будут добавлены](https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report) в GHC. Однако, зависимые типы ассоциируются с доказательством теорем и формализацией различных систем, так что их польза может быть неочевидной для рядового программиста. В этом посте мы рассмотрим зависимые типы на примере простой программистской задачи -- типобезопасного взаимодействия с [Яндекс.Директ API v5](https://tech.yandex.ru/direct/doc/dg/concepts/about-docpage/).

Популяризации зависимых типов для программирования немало способствует библиотека Servant, используемая как для предоставления типизированного API (servant-server), так и для написания запросов к уже существующему (servant-client). Мы будем использовать эту библиотеку (объясняние принципа ее работы выходит за рамки данного поста, желающие могут ознакомиться с докладом [Делаем свою жизнь проще с Servant](https://ruhaskell.org/posts/talks/2016/04/05/better-services-with-servant.html)). Однако, встроенными в Servant зависимыми типами дело не ограничится, и мы добавим еще своих.

[Репозиторий с кодом](https://github.com/effectfully/yandex-direct).

# План

Мы опишем несколько базовых сущностей Яндекс.Директ API: метод (add, get), операция, результат операции, объект (наименования словарей, объявления, кампании) и определим зависимо-типизированную функцию, которая позволяет вызвать любой метод, параметризованный некоторым объектом, и принять ответ, тип которого зависит от вызванного метода и типа объекта.

# Операции

Яндекс.Директ API  [предоставляет](https://tech.yandex.ru/direct/doc/dg/best-practice/principles-docpage/) довольно много методов, которые можно выполнять, поэтому выберем небольшое подмножество (я буду опускать `deriving (...)` везде):

```haskell
data Method = Add | Get
```

Операцией является метод с переданными ему параметрами:

```haskell
data Operation a = Operation
  { method :: !Method
  , params :: !a
  }
```

Каждая операция возвращает некоторый результат:

```haskell
data Result r = Result
  { result :: !r
  }
```

Разные методы возвращают значения разных типов. В случае использования метода `add`, возвращается структура `AddResults`:

```haskell
data AddResults = AddResults
  { getAddResults :: [ActionResult]
  }

data ActionResult = ActionResult
  { getId       :: !(Maybe Integer)
  , getWarnings :: !(Maybe [ExceptionNotification])
  , getErrors   :: !(Maybe [ExceptionNotification])
  }

data ExceptionNotification = ExceptionNotification
  { getCode    :: !Int
  , getMessage :: !Text
  , getDetails :: !(Maybe Text)
  }
```

Если же используется метод `get`, то результат зависит от параметра, с которым метод был вызван. К примеру, если методу `get` был передан параметр типа `DictionaryNames` (по сути список названий словарей)

```haskell
data DictionaryNames = DictionaryNames
  { getDictionaryNames :: [DictionaryNameEnum]
  }

data DictionaryNameEnum
  = GeoRegions
  | SupplySidePlatforms
  | ...
```
  
то результатом будет `Dictionaries` (тип, изоморфный гетерогенному списку всех словарей, каждый из которых завернут в `Maybe`, так что для каждого словаря, который не был запрошен, будет возвращено `Nothing`):

```haskell
data Dictionaries = Dictionaries
  { getGeoRegions          :: !(Maybe [GeoRegionsItem])
  , getSupplySidePlatforms :: !(Maybe [SupplySidePlatformsItem])
  , ...
  }

data GeoRegionsItem = GeoRegionsItem
  { getGeoRegionId   :: !Integer
  , getGeoRegionName :: !Text
  , getGeoRegionType :: !Text
  , getParentId      :: !(Maybe Integer)
  }

data SupplySidePlatformsItem = SupplySidePlatformsItem
  { getTitle :: !Text
  }
```

Таким образом тип возвращаемого значения зависит от вызванного метода и от типа параметра, ему переданного. В коде это выглядит следующим образом:

```haskell
type family ResultOf m a where
  ResultOf 'Add a               = AddResults
  ResultOf 'Get DictionaryNames = Dictionaries
```

Поскольку методы являются значениями, а Пи-типов в Haskell еще нет, мы должны использовать технику под названием singleton types, чтобы поднять на уровень типов то, что существует на уровне значений (на эту тему тоже был [пост](https://ruhaskell.org/posts/theory/2016/01/06/serialization-with-deptypes.html)):

```haskell
data SMethod m where
  SAdd :: SMethod 'Add
  SGet :: SMethod 'Get

evalSMethod :: SMethod m -> Method
evalSMethod SAdd = Add
evalSMethod SGet = Get
```

# Объекты

Яндекс.Директ API является весьма разветвленным: с его помощью можно обрабатывать объявления, группы объявлений, кампании, расширения, ключевые слова и многое другое. Мы напишем обобщенную функцию, которая позволит передать вызываемому методу любой из этих объектов.

Сначала пару примеров. Объявления:

```haskell
data Ads = Ads
  { getAds :: [AdAddItem]
  }

data AdAddItem = ...
```

Кампании:

```haskell
data Campaigns = Campaigns
  { getCampaigns :: [CampaignAddItem]
  }

data CampaignAddItem = ...
```

Адресом для обработки запросов, имеющих отношение к объявлениям, является "https://api.direct.yandex.com/json/v5/ads", а адресом для обработки запросов, имеющих отношение к кампаниям, является "https://api.direct.yandex.com/json/v5/campaigns". Все остальные объекты также имеют собственные адреса для обработки запросов. Так что заведем класс типов

```haskell
class ToJSON a => Entity a where
  entityName :: a -> String

instance Entity Ads where
  entityName _ = "ads"

instance Entity Campaigns where
  entityName _ = "campaigns"
```

То есть каждый объект должен быть сериализуем в JSON, чтобы его можно было передать по сети, и иметь имя, которое составляет часть адреса для запросов.

# API

Опишем API Директа с помощью инструментов, предоставляемых Servant:

```haskell
-- https://tech.yandex.ru/direct/doc/dg/concepts/headers-docpage/
type DirectAPI a r = Header "Authorization"   Text
                  :> Header "Accept-Language" Text
                  :> Header "Client-Login"    Text
                  :> ReqBody '[JSON] (Operation a)
                  :> Post    '[JSON] (Result    r)
```

`a` -- тип параметра вызываемого метода, `r` -- тип возвращаемого результата. Операция автоматически сериализуется в JSON, результат автоматически десериализуется из JSON.

Нам также понадобится Директ-специфичный конфиг:

```haskell
data DirectConfig = DirectConfig
  { getToken :: !Text         -- https://tech.yandex.ru/direct/doc/dg/concepts/auth-token-docpage/
  , getLogin :: !(Maybe Text)
  , getHost  :: !Text         -- Может быть как "api.direct.yandex.com",
                              --          так и "api-sandbox.direct.yandex.com"
  }                  
```

И мы наконец можем собрать все вместе и определить основную функцию для общения с API Директа:
  
```haskell
perform :: (FromJSON (ResultOf m a), Entity a)
        => DirectConfig -> Manager -> SMethod m -> a -> ClientM (ResultOf m a)
perform (DirectConfig token login host) manager smethod entity = result <$> run where
  proxy = Proxy :: Proxy (DirectAPI a r)
  auth  = Just $ "Bearer " `mappend` token
  lang  = Just "ru"
  oper  = Operation (evalSMethod smethod) entity
  url   = BaseUrl Https (unpack host) 443 $ "/json/v5/" ++ entityName entity
  run   = client proxy auth lang login oper manager url
```

(В новых версиях Servant `Manager` является частью `ClientM`, но на момент написания поста в последнем LTS-снимке на Stackage используется старая версия). `perform` принимает Директ-специфичный конфиг, менеджер соединения, метод, который следует вызвать, в форме синглтона и объект, который будет передан методу. Из сигнатуры `perform` видно, что тип возвращаемого значения (`ResultOf m a`) действительно зависит от вызванного метода (`m` в `SMethod m`) и от типа параметра (`a`), ему переданного. В остальном мы просто используемым базовый функционал, предоставляемый Servant.

# Заключение

С помощью зависимо-типизированного Servant можно декларативно описать API, и избавиться от необходимости вручную сериализовать-десериализовать данные. Однако, типы данных, приходящих в ответ на запрос, могут зависеть от содержимого запроса, но зависимые типы и здесь позволяют выразить все необходимые инварианты.
