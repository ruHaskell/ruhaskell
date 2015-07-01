---
author:         afiskon
title:          Работа с конфигурационными файлами в Haskell
tags:           конфигурационные файлы, configurator
description:    Ни одно достаточно крупное приложение не обходится без конфигурационных файлов. Познакомимся с конфигуратором.
hrefToOriginal: http://eax.me/haskell-configs/
---

Ни одно достаточно крупное приложение не обходится без конфигурационных файлов. Сегодня мы познакомимся с пакетом configurator, который позволяет не только банально парсить конфиги в Haskell, но и делать другие интересные вещи. Например, узнавать об изменении конфигов и автоматически перечитывать их. Автором пакета является широко известный в узких кругах Bryan O’Sullivan.

Создадим у себя в домашнем каталоге простой конфиг с именем test.conf:

```yaml
database {
  host = "127.0.0.1"
  name = "mydatabase"
  user = "admin"
  pass = "qwerty"
}
```

Как видите, синтаксис конфига прост и понятен. Также в конфиге можно импортировать другие конфиги:

```yaml
import "$(HOME)/someapp.conf"
```

Основные функции для работы с конфигурационными файлами следующие:

```haskell
empty :: Config
```

Создаем новый пустой конфиг.

```haskell
load :: [Worth FilePath] -> IO Config
```

Читаем конфигурационные файлы с диска. Тип `Worth` определяется примерно как `data Worth a = Required a | Optional a`.

```haskell
subconfig :: Name -> Config -> Config
```

Получение части конфига.

```haskell
reload :: Config -> IO ()
```

Перечитываем конфиги. При этом конфиги, полученные с помощью функции `subconfig`, также обновляются.

```haskell
lookup :: Configured a => Config -> Name -> IO (Maybe a)
```

Чтение параметра из конфига.

```haskell
lookupDefault :: Configured a => a -> Config -> Name -> IO a
```

Аналогично предыдущей функции, только в случае отсутствия параметра будет использовано значение по умолчанию.

```haskell
require :: Configured a => Config -> Name -> IO a
```

Аналогично `lookup`, только в случае отсутствия параметра вместо того, чтобы вернуть `Nothing`, будет брошено исключение.

```haskell
display :: Config -> IO ()
```

Вспомогательная функция для вывода конфига.

```haskell
getMap :: Config -> IO (HashMap Name Value)
```

Преобразуем конфиг в простой `HashMap`. Удобно, например, для последующей передачи этого `HashMap` в чистые функции.

Теперь дружно скажем `cabal install configurator` и запустим `ghci`:

```bash
ghci> import qualified Data.Configurator as C
ghci> import qualified Data.Text as T
```

Загрузим наш конфиг:

```bash
ghci> conf <- C.load [C.Required "$(HOME)/test.conf"]
```

Выведем его на экран:

```bash
ghci> C.display conf
("",fromList [("database.user",String "admin"),("database.name",String "mydatabase"),("database.pass",String "qwerty"),("database.host",String "127.0.0.1")])
```

Получим имя пользователя:

```bash
ghci> let key = T.pack "database.user"
ghci> user <- C.lookup conf key :: IO (Maybe String)
ghci> user
Just "admin"
```

Попробуем поработать с сабконфигом:

```bash
ghci> let subconf = C.subconfig (T.pack "database") conf
ghci> pass <- C.require subconf (T.pack "pass") :: IO String
ghci> pass
"qwerty"
```

... и использовать значения по умолчанию:

```bash
ghci> port <- C.lookupDefault 5432 subconf (T.pack "port") :: IO Int
ghci> port
5432
```

Совсем не сложно, правда?

Теперь попробуем разобраться с автоматическим обновлением конфигов. Типы и функции, которые нам понадобятся, следующие:

```haskell
data AutoConfig = AutoConfig {
      interval :: Int
    , onError :: SomeException -> IO ()
    }
```

Этот тип определяет, как часто следует перечитывать конфиг и что делать в случае возникновения ошибки.

```haskell
autoConfig :: AutoConfig
```

Возвращает значение по умолчанию типа `AutoConfig`. Интервал равен одной секунде, ошибки игнорируются.

```haskell
autoReload :: AutoConfig -> [Worth FilePath] -> IO (Config, ThreadId)
```

Создает автоматически обновляемый конфиг.

Посмотрим на все это хозяйство в действии. Говорим:

```bash
ghci> let paths = [C.Required "/home/eax/test.conf"]
ghci> (conf2, _) <- C.autoReload C.autoConfig paths
ghci> C.getMap conf2
fromList [("database.user",String "admin"), ...
```

Открываем конфиг, заменяем имя пользователя на «guest», снова говорим:

```bash
ghci> C.getMap conf2
fromList [("database.user",String "guest"), ...
```

Кажется, работает! Ну, почти. На момент написания этих строк в `configurator `было несколько неисправленных багов. Например, если использовать в пути к конфигу переменную `$(HOME)`, он не будет обновляться. Ознакомиться со списком известных багов можно [здесь](https://github.com/bos/configurator/issues).

Также configurator позволяет подписаться на обновления определенных частей конфига.


```haskell
prefix :: Text -> Pattern
```

Возвращает шаблон для подписки на определенную часть конфига.

```haskell
exact :: Text -> Pattern
```

Создает шаблон для подписки на один конкретный параметр.

```haskell
type ChangeHandler = Name -> Maybe Value -> IO ()
```

Тип функции, вызываемой при внесении изменений в конфиге.

```haskell
subscribe :: Config -> Pattern -> ChangeHandler -> IO ()
```

Подписаться на изменения.

Проверяем:

```bash
ghci> :{
Prelude C T| let changeHandler name val = do
Prelude C T|       putStrLn $ show name ++ " changed to " ++ show val
Prelude C T| :}
ghci> let prfx = (C.prefix $ T.pack "database")
ghci> let exct = (C.exact $ T.pack "database.port")
ghci> C.subscribe conf2 prfx changeHandler
ghci> C.subscribe conf2 exct changeHandler
```

Открываем конфиг и дописываем в него строчку:

```yaml
  port = 5432
```

В ghci видим:

```bash
ghci> "database.port" changed to Just (Number (5432 % 1))
"database.port" changed to Just (Number (5432 % 1))
```

Вот такой занятный пакет этот `configurator`. А чем вы парсите конфиги в Haskell?

