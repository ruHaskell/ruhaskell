---
author:         Дмитрий Долгов
title:          Парсинг бинарных данных, part I
tags:           бинарные данные, npy
description:    Первый шаг на пути создания библиотеки для работы с npy файлами в Haskell
---

Так уж сложилось, что у меня, как у разработчика, развоение личности - одна из них живет по Zen of Python, другая тяготеет к Haskell. И самым большим разочарованием этой драмы является наличие *numpy* в экосистеме Python, и отсутствие его в экосистеме Haskell. Есть [hmatrix](http://dis.um.es/~alberto/hmatrix/hmatrix.html) и другие интересные библиотеки, но все же.

Однако есть большие надежды на то, что это изменится, ну а пока я решил попробовать сделать первый шаг в этом направлении, и реализовать библиотеку для работы с файлами в формате *npy*. Это достаточно простой и удобный [формат](https://github.com/numpy/numpy/blob/master/doc/neps/npy-format.rst), который я в последнее время использую для хранения расчетных данных.

![Npy format](http://i58.tinypic.com/34dr2md.png "Npy format")

Для начала немного упростим задачу и будем просто выводить результат разбора одномерного массива данных, сохраненного в npy формате (например, ```[0.1, 0.2, 0.3, 0.4]```).

Для начала определимся с используемыми типами данных:

```haskell
data Header = Header {
    magic           :: BC.ByteString,
    minVersion      :: Word8,
    maxVersion      :: Word8,
    headerLen       :: Integer,
    header          :: BC.ByteString,
    npyData         :: [Double]
    } deriving (Show)

data DictHeader = DictHeader {
    descr           :: String,
    fortran_order   :: Bool,
    shape           :: [Int]
    } deriving (Show)
```

*Header* будет содержать данные о файле в целом:

* magic (6 byte) -> всегда ``` "x93NUMPY" ```
* maxVersion (1 byte) -> основная цифра версии формата файла
* minVersion (1 byte) -> минорная цифра версии формата файла
* headerLen (2 byte) -> длина заголовка файла
* header -> собственно, заголовок 

*DictHeader* представляет данные, которые хранятся в ```Header.header```.

Для извлечения данных из файла будем использовать пакет [cereal](http://hackage.haskell.org/package/cereal-0.4.1.1), т.к. обычно используемая для этих целей библиотека [binary](https://hackage.haskell.org/package/binary), на удивление, не умеет работать с числами с плавающей точкой.

```haskell
file <- openBinaryFile filePath ReadMode
input <- BL.hGetContents file
let npyParsedData = runGetLazy npyHeader input
case npyParsedData of
    Left msg -> print $ msg
    Right npyParsedData -> do
        let dataForParsing = BC.unpack . header $ npyParsedData
        let dictHeader = unwrap . header $ npyParsedData
        print $ header npyParsedData
        print $ dictHeader
        print $ npyData npyParsedData
```

В коде есть один интересный (```runGetLazy```) и один неинтересный момент (```unwrap```). Последняя предназначена для обработки заголовка, и она не использует готовые библиотеки для парсинга JSON. Дело в том, что это не совсем JSON, а python dictionary, поэтому ```Aeson``` и ```Text.JSON``` не хотят с ним работать. Но т.к. формат его достаточно прост и вряд ли будет меняться, я просто выдрал нужные данные из строки без посредников.

```runGetLazy``` возвращает либо итоговые данные, либо сообщение об ошибке (```Either String a```), и использует следующую функцию:

```haskell
npyHeader :: Get Header

getNpyData = do
    empty <- isEmpty
    if empty
        then return []
        else do 
            v <- getFloat64le
            rest <- getNpyData
            return (v : rest)

npyHeader = do
    magic <- getByteString 6
    minVersion <- getWord8
    maxVersion <- getWord8
    headeLen <- getWord16le
    header <- getByteString $ fromIntegral headeLen
    npyData <- getNpyData
    return Header {
        magic=magic,
        minVersion=minVersion,
        maxVersion=maxVersion,
        headeLen=fromIntegral headeLen,
        header=header,
        npyData=npyData
    }
```

Собственно, для каждого поля мы получаем необходимые данные. Для чтения самого массива чисел, который идет после заголовка, мы используем отдельную функцию, которая (пока!) вытягивает все до последнего без каких-либо проверок, но для простого случая этого достаточно.

Что удивительно, на этом все =) Полный код можно подсмотреть [здесь](https://github.com/erthalion/hnpy). Теперь, если мы создадим тестовый файл (можно взять из репозитория, или создать самостоятельно ```np.save('test', np.array([0.1, 0.2, 0.3, 0.4]))```), и натравим на него нашу утилитку, мы получим заложенные в нем данные (здесь были белки-истерички, но в финальной версии статьи я их удалил =):

```
$ hnpy test.npy

"{'descr': '<f8', 'fortran_order': False, 'shape': (4,), }            \n"
DictHeader {descr = "<f8", fortran_order = False, shape = [4]}
[0.1,0.2,0.3,0.4]
```

Отдаю на растерзание, буду благодарен за правки и рационализаторские предложения.
