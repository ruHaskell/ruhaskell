---
author: Александр Вершилов
title:  Реализация реляционной алгебры
tags: велосипед, типы
description: Пост описывает возможности построения реляционных отношений и базовых операций над ними.
---

Пока все умные люди занимаются обсуждением records и OFR мы попробуем в очередной
раз изобрести колесо и попытаться сделать гетерогенные записи.
Поскольку просто делать такие записи не интересно, то попытаемся так же
представить простенькую библиотеку для реляционной алгебры. Данный пост посвящен
в основном отработке подходов к работе с типами и не очень полезен с прикладной 
точки зрения.

Данный пост является [Literate Haskell](https://wiki.haskell.org/Literate_programming) файлом и может быть вставлен в редактор.
При написании поста использовался компилятор Glasgow Haskell Compiler версии 7.8 и на более
ранних версиях код не будет работать, во всяком случае без изменений.


Для начала загрузим немножно расширений:

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UnicodeSyntax #-}

И используемых модулей.

> import GHC.TypeLits
> import Data.Type.Equality
> import Data.Proxy

> import           Data.Set (Set)
> import qualified Data.Set as Set
> import           Data.Monoid
> import           Unsafe.Coerce


Для построения реляционной алгебры нам нужно будет уметь представлять множества
кортежей, где у каждого кортежа есть атрибуты определенного типа. Для представления
кортежей можно использовать различные способы, например, пакет [dependent-map](https://hackage.haskell.org/package/dependent-map),
или [гетерогенные списки](https://hackage.haskell.org/package/HList), но поскольку
велосипеды строить интересно, то ниже приведено построение гетерогенных списков,
работающих очень похоже на HList.

Для дальнейшей работы нам потребуется два типа, тут можно обойтись и одним, но
так проще. Для начала, введем тип данных, обозначающий, что поле "вида" `а`, имеет
тип `b`.

> newtype a :-> b = V b deriving (Eq, Show)

Здесь важно, что `:->` является оберткой типа или типом данных, а не синонимом типа,
поскольку это позволяет сохранять информацию о "теге" `a`.

Пример:

    *Main> V 5 :: "String" :-> Int
    V 5
    *Main> :t it
    it :: "String" :-> Int

Здесь мы построили тегированную запись и показали, что тип сохраняется.

Так же введем тип, отвечающий предыдущему на уровне типов

> data a :--> b 

Данное действие совершенно не обязательно, но так удобнее работать.



Теперь все готово для ввода кортежей,

> data HRec :: [*] -> * where
>   HNil  :: HRec '[]
>   HCons :: a -> HRec xs -> HRec ((b :-> a) ': xs)

Так мы построили кортеж в виде связного списка, каждый из узлов которого параметризован
типом $b \rightarrow a$, обозначающим, что имени `b` соответсвует значение типа `a`.
Для такой записи доступ к значению, к сожалению, будет линейным от размера записи,
но это когда-нибудь можно будет исправить.

Добавим полезный инстанс:

> instance Show (HRec '[]) where show _ = "HNil"
>
> instance (KnownSymbol s, Show (HRec xs), Show a) => Show (HRec ((s :-> a) ': xs)) where
>    show as@(HCons _ hs)  = "HCons (" ++ inner Proxy as ++ ") (" ++ show hs ++ ")"
>      where
>        inner :: (Show a, KnownSymbol s) => Proxy s -> HRec ((s :-> a)':xs) -> String
>        inner p (HCons a _) = symbolVal p ++ " :--> " ++ show a

Пример использования:

     *Main> let t = (HCons "q" (HCons 5 HNil) :: HRec ["Name" :-> String, "Age" :-> Int])
     *Main> t
     HCons (Name :--> "q") (HCons (Age :--> 5) (HNil))
     *Main> :t t
     t :: HRec '["Name" :--> String, "Age" :--> Int]


Для того, чтобы кортежи могли находиться во множествах, введем `Eq` и `Ord` инстансы:

> instance Eq (HRec '[]) where _ == _ = True
> instance (Eq b, Eq (HRec xs)) => Eq (HRec ((a :-> b) ': xs)) where
>   (HCons a as) == (HCons b bs) = a == b && as == bs
>
> instance Ord (HRec '[]) where compare _ _ = EQ
> instance (Ord b, Ord (HRec xs)) => Ord (HRec ((a :-> b) ': xs)) where
>    (HCons a as) `compare` (HCons b bs) = a `compare` b <> as `compare` bs


Для представления множеств используем обертку:

> newtype RelSet xs = RS { unRS :: Set (HRec xs) }

Тут вполне можно использовать и синоним типа, но это будет хуже, т.к. newtype является
ещё и прокси и может служить для передачи информации о типе. Если кто хочет, то
может самостоятельно проверить использование синонима типа в данном контексте.

Начнем с простых операций над множествами:

Объедиение, Пересечение, Вычитание
-----------------------------------

Данные операции работают над множествами с кортежами одинакового типа, и являются
обычными теорекомножественными операциями, поэтому не предоставляют большого
интереса, но все же с них проще начинать.

> union :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> union (RS sx) (RS sy) = RS $ sx `Set.union` sy

> intersection :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> intersection (RS sx) (RS sy) = RS $ sx `Set.intersection` sy

> subtraction :: Ord (HRec xs) => RelSet xs -> RelSet xs -> RelSet xs
> subtraction (RS sx) (RS sy) = RS $ sx Set.\\ sy


Переименование
--------------

Переименованием называется унарная операция $\rho_{a / b}(R)$ результатом которой является
множество равное $R$, за исключением того, что все аттрибуты `a` в кортежах переименованы в `b`. 

Рассмотрим сначала переименование на типах, т.е. если у нас есть список на уровне типов 
`[s -> a] :: [*]` и список переименований `[s -> g] :: [*]` и если $s_n = g_n$, то тогда
мы переименовываем $s_n -> a \rightarrow g_n -> a$. Это можно сделать рекурсивно, для
каждого из элементов списка $G$ мы проходим по всем элементам списка `a`, переименовывая если имена совпадают.

> type family Rename (a :: [*]) (b :: [*]) :: [*] where
>        Rename '[]       bs = '[]
>        Rename (a ': as) bs = RenameInner a bs ': Rename as bs

> type family RenameInner (a :: *) (b :: [*]) :: * where
>        RenameInner     a         '[]           = a
>        RenameInner (s :--> a) (s :--> g ': bs) = g :--> a
>        RenameInner     a      (    g    ': bs) = RenameInner a bs


Примеры использования:

     *Main> :kind! Rename ["Foo" :--> Int,"Bar" :--> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"]
     Rename ["Foo" :--> Int,"Bar" :--> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"] :: [*]
     = '["Bar" :--> Int, "Foo" :--> ()]

Так же мы можем использовать это при определении записей:

    *Main> show (HCons 5 (HCons () HNil) :: HRec (Rename ["Foo" :--> Int,"Bar" :--> ()] ["Foo" :--> "Bar", "Bar" :--> "Foo"]))
    "HCons (Bar :--> 5) (HCons (Foo :--> ()) (HNil))"

Теперь надо научиться переименовывать записи на уровне значений.
Для этого введем вспомогательную функцию. Данная функция конструирует 
прокси, описывающий тип значения, которое получится после переименования.

> rrename :: Rename' HRec a (Rename a b) => HRec a -> proxy b -> HRec (Rename a b)
> rrename h r = rrename' h (pp h r)
>   where pp :: proxy1 a -> proxy2 b -> Proxy (Rename a b)
>         pp _ _ = Proxy

Здесь для передачи типов используется явный конструктор прокси,
что во многих случаях может быть заменено использованием расширения
`ScopedTypeVariables`.

> class Rename' s (a :: [*])  (b :: [*]) where
>   rrename' :: s a -> proxy b -> s b
>
> instance Rename' HRec '[] '[] where
>   rrename' HNil _ = HNil
>
> instance Rename' HRec as bs => Rename' HRec ((x :-> a) ': as) ((y :-> a) ': bs) where
>   rrename' (HCons a as) p = HCons a (rrename' as (pp p))
>     where pp :: proxy (a ': as) -> Proxy as
>           pp _ = Proxy

Пример:

    *Main> rrename t (Proxy :: Proxy '["Foo" :-> "Zoo"])
    HCons (Zoo :--> 5) (HCons (Bar :--> ()) (HNil))


Но как внимательный читатель уже заметил, представление соверешенно не изменяется,
поэтому справедлива будет следующая реализация:

> rename'' :: HRec a -> proxy b -> HRec (Rename a b)
> rename''  h _ = unsafeCoerce h

     *Main> rrename'' t (Proxy :: Proxy '["Foo" :--> "Zoo"])
     HCons (Zoo :-> 5) (HCons (Bar :-> ()) (HNil))

Задание. Проверим, что будет если мы попробуем переименовать несуществующее поле:

     *Main> :kind! Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "E", "Zoo" :--> "Foo"]
     Rename ["Foo" :-> Int,"Bar" :-> ()] ["Foo" :--> "E", "Zoo" :--> "Foo"] :: [*]
     = '["E" :-> Int, "Bar" :-> ()]

Как изменить код таким образом, чтобы такое переименование было запрещено (не выводилось)?

Собственно, теперь можно представить саму реализацию метода:

> ρ :: RelSet a -> proxy b -> RelSet (Rename a b)
> ρ  r _ = unsafeCoerce r

Проекция
--------

При выполнении проекции выделяется «вертикальная» вырезка отношения-операнда с естественным уничтожением потенциально возникающих кортежей-дубликатов.


Как обычно, начнем с функции, работающей на уровне типов:

> type family Project' a b where
>   Project' s       '[]     = '[]
>   Project' s    (b ': bs)  = ProjectInner' s b ': Project' s bs 
>
> type family ProjectInner' s b where
>   ProjectInner' (s :-> a ': as) s = s :-> a
>   ProjectInner' (   a    ': as) s = ProjectInner' as s

Пример:

     *Main> :kind! Project' ["Foo" :-> Int,"Bar" :-> ()] '["Foo"]
     Project' ["Foo" :-> Int,"Bar" :-> ()] '["Foo"] :: [*]
     = '["Foo" :-> Int]

Для построения проекции сначала постоим вспомогательную функцию,
которая по имени атрибута достает значение из кортежа:
 
> rlookupP :: Lookup s a b c => s a -> Proxy b -> c
> rlookupP s b = rlookup' Proxy b s 


Опять же, строим класс для итерирования по структуре. Данный
класс является более полиморфным, чем необходимо, т.к. в данном
случае он полиморфен по структуре, к которой делаются запросы.
Так же этот класс использует функциональные зависимости, которые
обозначают, что параметры типа `s a b` однозначно определяют тип `c`.

> class Lookup s a b c | s a b -> c where
>   rlookup' :: Proxy a -> Proxy b -> s a -> c 
> 
> instance Lookup HRec ( (n :-> a) ': xs ) n a where
>   rlookup' _ _ (HCons a _) = a
> 
> instance Lookup HRec xs n c => Lookup HRec ( (m :-> a) ': xs ) n c where
>   rlookup' pa pb (HCons _ xs) = rlookup' (pa' pa) pb xs
>     where
>       pa' :: Proxy (a ': xs) -> Proxy xs
>       pa' _ = Proxy


При помощи вспомогательного класса можно построить саму проекцию.

> class Project s a b c | s a b -> c where
>   rproject :: s a -> Proxy b -> s c 
> 
> instance Project HRec a '[] '[] where
>   rproject _ _ = HNil
> 
> instance (Project HRec a bs c, Lookup HRec a b k) => Project HRec a (b ': bs) ((b :-> k) ': c) where
>   rproject xs p = rcons (pHead p)
>                         (rlookupP xs (pHead p))
>                         (rproject xs (pTail p))
>     where pHead :: Proxy (b ': bs) -> Proxy b
>           pHead _ = Proxy
>           pTail :: Proxy (b ': bs) -> Proxy bs
>           pTail _ = Proxy

> rcons :: Proxy b -> a -> HRec c -> HRec ( (b :-> a) ': c)
> rcons _ a xs = HCons a xs

Пример:

      *Main> rproject t (Proxy :: Proxy '["Bar"])
      HCons (Bar :-> ()) (HNil)


Теперь сделаем такую же операцию для множеств, заодно убрав прокси:


> π :: (Project HRec xs ys ys, Ord (HRec ys)) => RelSet xs -> RelSet ys
> π rs = inner Proxy rs 
>   where inner :: (Project HRec xs ys ys, Ord (HRec ys)) => Proxy ys -> RelSet xs -> RelSet ys
>         inner p (RS sx) = RS $ Set.map (\x -> rproject x p) sx

> projection :: (Project HRec xs ys ys, Ord (HRec ys)) => RelSet xs -> RelSet ys
> projection = π 

Выборка
-------

Для выборки придется немного пофантазировать. И сначалы мы реализуем частный
случай для выборки, когда условия можно записать в виде `a && b && c`, где `a`,`b`,`c`,
условия на соотвествующие атрибуты.

Для этого введем класс предикатов по кортежу:

> class RPredicate s a b where
>   rpredicate :: s a -> s b -> Bool

И предикатов по конкретному типу:

> class RPredicate1 s a b where
>   rpredicate1 :: s a -> b -> Bool

> instance RPredicate HRec s '[] where
>   rpredicate _ _ = True

> instance (RPredicate HRec s ps, RPredicate1 HRec s (n :-> (a -> Bool))) => RPredicate HRec s (( n :-> (a -> Bool)) ': ps) where
>   rpredicate s h@(HCons _ xs) = rpredicate1 s (v h) && rpredicate s xs
>     where v :: HRec (n :-> (a -> Bool) ': b) -> (n :-> (a -> Bool))
>           v (HCons f _) = V f


> instance RPredicate1 HRec ((s :-> a) ': as) (s :-> (a -> Bool)) where
>   rpredicate1 (HCons a _) (V f) = f a

> instance RPredicate1 HRec as f => RPredicate1 HRec (a ': as) f where
>   rpredicate1 (HCons a as) f = rpredicate1 as f 

Задание. Как можно построить обобщенное решение, позволяющее конструировать произвольные
предикаты.

Теперь мы можем построить выборку из множества:

> σ :: (RPredicate HRec xs ys) => RelSet xs -> HRec ys -> RelSet xs
> σ (RS sx) h = RS $ Set.filter (\x -> rpredicate x h) sx

> selection :: (RPredicate HRec xs ys) => RelSet xs -> HRec ys -> RelSet xs
> selection = σ

Так же введем частный случай, предикат частичного совпадения:
если у нас есть две записи, одна из которых является подмножеством другой, то мы хотим
проверять совпадает ли их пересечение.

> type family EqPred z where
>   EqPred '[] = '[]
>   EqPred ((n :-> a) ': xs) = (n :-> (a -> Bool)) ': EqPred xs

> class EqPredicate s a b | s a -> b where
>   eqPredicate :: s a -> s b

> instance EqPredicate HRec '[] '[] where
>   eqPredicate HNil = HNil

> instance (EqPredicate HRec as bs, Eq a) => EqPredicate HRec ((n :-> a) ': as) ((n :-> (a -> Bool)) ': bs) where
>   eqPredicate (HCons x xs) = HCons (==x) (eqPredicate xs)


Произведение
-------------

Произведением множеств $R_1=\{a_1,\ldots,a_n\}$ и $R_2=\{b_1,\ldots,b_n\}$ называется
множество $R_3=\{a_1,\ldots,a_n,b_1,\ldots,b_n\}$ с прямым перебором всех элементов.

Как обычно, сначала пишем функцию работающую на уровне типов:

> type family Append xs ys where
>   Append '[] ys = ys
>   Append (x ': xs) ys = x ': Append xs ys

И затем конструируем классы типов для объединения:

> class Merge s a b c | s a b -> c where
>   rmerge :: s a -> s b -> s c
> 
> instance Merge HRec '[] b b where
>   rmerge HNil xs = xs
> 
> instance Merge HRec as b c => Merge HRec (a ': as) b (a ': c) where
>   rmerge (HCons x xs) s = HCons x (rmerge xs s)

> multiplication :: (Merge HRec xs ys (Append xs ys), Ord (HRec (Append xs ys))) => RelSet xs -> RelSet ys -> RelSet (Append xs ys)
> multiplication (RS sx) (RS sy) =
>   RS $ Set.fromList [ x `rmerge` y
>                     | x <- Set.toList sx
>                     , y <- Set.toList sy
>                     ]

Деление
-------

И напоследок деление, строящееся аналогичным образом:

> type family Minus xs ys where
>   Minus xs '[] = xs
>   Minus xs (y ': ys) = Minus (MinusInner xs y) ys
>
> type family MinusInner xs y where
>   MinusInner (y ': xs) y = xs
>   MinusInner (x ': xs) y = x ': MinusInner xs y

> division :: (Eq (RelSet ys), Ord (HRec ys), Project HRec xs ys ys, RPredicate HRec xs (EqPred (Minus xs ys)), EqPredicate HRec (Minus xs ys) (EqPred (Minus xs ys)), Ord (HRec (Minus xs ys)), Project HRec xs (Minus xs ys) (Minus xs ys)) => RelSet xs -> RelSet ys -> RelSet (Minus xs ys)
> division rx ry = RS $ Set.fromList [ s | (s,v) <- [ (x, projection (selection rx (eqPredicate x))) | x <- Set.toList (unRS $ projection rx)], v == ry]



Заключение
----------

Зачем все это может быть нужно? В данном случае -- просто так, для отработки работы
с типами и гетерогенными записями. Части похожих задач могут возникать в произвольных
местах, в проектах, и хорошо бы иметь представление о работе с ними.

Если код выше можно как-то обобщить или упростить или обсудить какой-то из вопросов
отдельно, то буду рад комментариям.
