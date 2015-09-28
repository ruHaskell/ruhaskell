{-
    Модуль, отвечающий за работу с тематическими тегами и с именами авторов статей.
    https://github.com/ruHaskell/ruhaskell
    Все права принадлежат русскоязычному сообществу Haskell-разработчиков, 2015 г.
-}

{-# LANGUAGE OverloadedStrings #-}

module Tags (
    buildPostsTags,
    buildPostsAuthors,
    buildPostsCategories,
    createPageWithAllTags,
    createPageWithAllAuthors,
    createPageWithAllCategories,
    convertTagsToLinks,
    convertCategoriesToLinks,
    convertAuthorsToLinks
) where

import Data.List            (intercalate, isInfixOf)
import Context              (postContext)
import Misc                 (TagsReader,
                             TagsAndAuthors,
                             getNameOfAuthor,
                             getRussianNameOfCategory)
import Control.Monad.Reader

import           Text.Blaze.Html                 (toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import Hakyll

-- Функция извлекает из всех статей значения поля tags и собирает их в кучу.
buildPostsTags :: MonadMetadata m => m Tags
buildPostsTags = buildTags "posts/**" $ fromCapture "tags/*.html"

-- Функция определяет категорию, к которой относится статья.
buildPostsCategories :: MonadMetadata m => m Tags
buildPostsCategories = buildCategories "posts/**" $ fromCapture "categories/*.html"

-- Функция извлекает из всех статей значения поля author и собирает их в кучу.
buildPostsAuthors :: MonadMetadata m => m Tags
buildPostsAuthors = buildTagsWith getNameOfAuthor "posts/**" $ fromCapture "authors/*.html"

-- Функция отрисовывает тег-ссылку вместе со значком, отражающим количество публикаций,
-- соответствующих данному тегу. Например, количество статей данного автора.
-- За основу взяты исходники Hakyll.
createTagLinkWithBadge :: Double
                       -> Double
                       -> String
                       -> String
                       -> Int
                       -> Int
                       -> Int
                       -> String
createTagLinkWithBadge = createGenericTagLinkWithBadge id

-- Отрисовываем тег для категории, с заменой родного английского названия русским аналогом.
createRussianTagLinkWithBadge :: Double
                              -> Double
                              -> String
                              -> String
                              -> Int
                              -> Int
                              -> Int
                              -> String
createRussianTagLinkWithBadge = createGenericTagLinkWithBadge getRussianNameOfCategory

createGenericTagLinkWithBadge :: (String -> String)
                              -> Double
                              -> Double
                              -> String
                              -> String
                              -> Int
                              -> Int
                              -> Int
                              -> String
{-# INLINE createGenericTagLinkWithBadge #-}
createGenericTagLinkWithBadge convert
                              smallestFontSizeInPercent
                              biggestFontSizeInPercent
                              tag
                              url
                              count
                              min'
                              max' =
    let diff     = 1 + fromIntegral max' - fromIntegral min'
        relative = (fromIntegral count - fromIntegral min') / diff
        size     = floor $ smallestFontSizeInPercent + relative * (biggestFontSizeInPercent - smallestFontSizeInPercent) :: Int
    in renderHtml $
        -- Формируем стандартный тег <a href...>
        H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
            ! A.href (toValue url)
            $ H.preEscapedToHtml $ convert tag
                                   ++ "<span class=\"badge-for-tag-link\">"
                                   ++ show count
                                   ++ "</span>"

-- Отрисовываем облако с тегами-ссылками, имеющими количественные значки.
renderTagCloudWithBadges :: Double
                         -> Double
                         -> Tags
                         -> Bool
                         -> Compiler String
renderTagCloudWithBadges smallestFontSizeInPercent
                         biggestFontSizeInPercent
                         specificTags
                         thisIsCategoriesCloud =
    let tagLinkRenderer = if thisIsCategoriesCloud
                          then createRussianTagLinkWithBadge
                          else createTagLinkWithBadge
    in
    renderTagCloudWith tagLinkRenderer
                       concatenateLinksWithSpaces
                       smallestFontSizeInPercent
                       biggestFontSizeInPercent
                       specificTags
    where concatenateLinksWithSpaces = intercalate ", "

-- Вспомогательная функция, формирующая страницу с облаком определённых тегов.
createPageWithTagsCloud :: Tags
                        -> Identifier
                        -> Double
                        -> Double
                        -> String
                        -> String
                        -> Identifier
                        -> Rules ()
createPageWithTagsCloud specificTags
                        pageWithSpecificTags
                        smallestFontSizeInPercent
                        biggestFontSizeInPercent
                        pageTitle
                        cloudName
                        specificTemplate =
    create [pageWithSpecificTags] $ do
        route idRoute
        compile $ do
            let renderedCloud _ = renderTagCloudWithBadges smallestFontSizeInPercent
                                                           biggestFontSizeInPercent
                                                           specificTags
                                                           ("Категории" `isInfixOf` pageTitle)
                tagsContext = mconcat [ constField "title" pageTitle
                                      , field cloudName renderedCloud
                                      , defaultContext
                                      ]

            makeItem "" >>= loadAndApplyTemplate specificTemplate tagsContext
                        >>= loadAndApplyTemplate "templates/default.html" tagsContext
                        >>= relativizeUrls

-- Формируем страницу с облаком тематических тегов.
createPageWithAllTags :: TagsReader
createPageWithAllTags = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (head tagsAndAuthors)
                                   "tags.html"
                                   110
                                   220
                                   "Темы публикаций"
                                   "tagsCloud"
                                   "templates/tags.html"
    return ()

-- Формируем страницу с облаком категорий.
createPageWithAllCategories :: TagsReader
createPageWithAllCategories = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (tagsAndAuthors !! 1)
                                   "categories.html"
                                   110
                                   220
                                   "Категории публикаций"
                                   "categoriesCloud"
                                   "templates/categories.html"
    return ()

-- Формируем страницу с облаком авторов публикаций.
createPageWithAllAuthors :: TagsReader
createPageWithAllAuthors = do
    tagsAndAuthors <- ask
    lift $ createPageWithTagsCloud (tagsAndAuthors !! 2)
                                   "authors.html"
                                   110
                                   220
                                   "Наши авторы"
                                   "authorsCloud"
                                   "templates/authors.html"
    return ()

convertSpecificTagsToLinks :: TagsAndAuthors
                           -> Tags
                           -> String
                           -> Rules ()
convertSpecificTagsToLinks tagsAndAuthors specificTags aTitle =
    tagsRules specificTags $ \tag pattern -> do
        let nameOfTag = if "категории" `isInfixOf` aTitle then getRussianNameOfCategory tag else tag
            title = renderHtml $ H.preEscapedToHtml $ aTitle ++ " " ++ nameOfTag
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let taggedPostsContext = mconcat [ listField "posts" (postContext tagsAndAuthors) (return posts)
                                             , constField "title" title
                                             , defaultContext
                                             ]

            makeItem "" >>= loadAndApplyTemplate "templates/posts.html" taggedPostsContext
                        >>= loadAndApplyTemplate "templates/default.html" taggedPostsContext
                        >>= relativizeUrls

-- Делаем тематические теги ссылками, что позволит отфильтровать статьи по тегам.
convertTagsToLinks :: TagsReader
convertTagsToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors
                                      (head tagsAndAuthors)
                                      "Все статьи по теме"
    return ()

-- Делаем названия категорий ссылками, что позволит отфильтровать статьи по категориям.
convertCategoriesToLinks :: TagsReader
convertCategoriesToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors
                                      (tagsAndAuthors !! 1)
                                      "Все статьи в категории"
    return ()

-- Делаем имена авторов ссылками, что позволит отфильтровать статьи по авторам.
convertAuthorsToLinks :: TagsReader
convertAuthorsToLinks = do
    tagsAndAuthors <- ask
    lift $ convertSpecificTagsToLinks tagsAndAuthors
                                      (tagsAndAuthors !! 2)
                                      "Все статьи автора"
    return ()
