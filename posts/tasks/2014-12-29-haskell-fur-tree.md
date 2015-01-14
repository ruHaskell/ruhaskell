---
author:         Алексей Пирогов
title:          "Ёлочка"
tags:           ёлочка
description:    Данная задача представляет собой одну из множества задач, задаваемых время от времени начинающим разработчикам на собеседованиях.
hrefToOriginal: https://www.fpcomplete.com/user/astynax/basics/pine-tree
---

Данная задача представляет собой одну из множества задач, задаваемых время от времени начинающим разработчикам на собеседованиях.

Само задание звучит так:

> Написать программу, выводящую в консоль для заданного числа n "изображение" ёлочки высотой n

Обычно, ожидается, что ёлка будет выглядеть так:

```
    *
   ***
  *****
 *******
*********
```

Данная задача просто решается на императивных ЯП, таких как Python. Здесь же будет приведено "функциональное" решение.

### Скелет программы

Скелет программы будет следующим:

```haskell
module Main where

main :: IO ()
main = putStrLn (pineTree 10)

pineTree :: Int -> String
pineTree = undefined
```

### Простое решение

Функция формирования "изображения" дерева представлена следующим кодом:

```haskell
pineTree x = unlines $ map row ranges
  where
    row (n, m) = take n spaces ++ take m stars
    stars  = repeat '*'
    spaces = repeat ' '
    ranges = zip [x - 1, x - 2 .. 0] [1, 3..]

main = putStrLn (pineTree 10)
```

### Бесточечный вариант

В качестве разминки для ума можно переписать функцию "рисования" ёлки в *бесточечном стиле*:

```haskell
pineTree = unlines
         . zipWith (flip (++)) (zipWith take [1,3..] (repeat (repeat '*')))
         . zipWith (flip take) (repeat (repeat ' '))
         . reverse
         . flip ($) [1,2..]
         . take

main = putStrLn (pineTree 10)
```

### Бесточечный вариант №2

Функция записана бесточечно, но сложновата на вид, хоть и повторяет изначальное решение. Однако можно реализовать эту функцию и короче:

```haskell
pineTree = unlines
         . takeWhile ((== ' ') . head)
         . iterate ((++ "**") . tail)
         . (++ "*")
         . (flip take) (repeat ' ')

main = putStrLn (pineTree 10)
```
