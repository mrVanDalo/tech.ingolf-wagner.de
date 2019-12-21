--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.Maybe (isNothing, isJust)
import Hakyll
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $
    -- copy all images
   do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    match "static/*" $ do
      route idRoute
      compile copyFileCompiler
    -- css files
    match "src/lessc/page/main.less" $ do
      route $ customRoute $ const "src/main.css"
      compile $
        getResourceString >>=
        withItemBody
          (unixFilter "lessc" ["-", "--include-path=./src/lessc/page/"]) >>=
        return . fmap compressCss
    match "src/lessc/remark/main-dark.less" $ do
      route $ customRoute $ const "src/remark-dark.css"
      compile $
        getResourceString >>=
        withItemBody
          (unixFilter "lessc" ["-", "--include-path=./src/lessc/remark/"]) >>=
        return . fmap compressCss
    match "src/lessc/remark/main-light.less" $ do
      route $ customRoute $ const "src/remark-light.css"
      compile $
        getResourceString >>=
        withItemBody
          (unixFilter "lessc" ["-", "--include-path=./src/lessc/remark/"]) >>=
        return . fmap compressCss
    -- slides
    matchMetadata
      "slides/**"
      (\meta ->
         case lookupString "draft" meta of
           Just "false" -> True
           Nothing -> True
           _ -> False) $ do
      route $ setExtension "html"
      compile $
        getResourceBody >>=
        loadAndApplyTemplate "templates/remarkjs.html" postCtx >>=
        relativizeUrls
    -- the new content out there
    matchMetadata
      "content/**"
      (\meta ->
         case lookupString "draft" meta of
           Just "false" -> True
           Nothing -> True
           _ -> False) $ do
      route $
        gsubRoute "content/" (const "") `composeRoutes` setExtension "html"
      compile $
        pandocCompilerWithToc >>=
        loadAndApplyTemplate "templates/layout.html" postCtx >>=
        relativizeUrls
    match "index.markdown" $ do
      route $ setExtension "html"
      compile $ do
        newContent <- loadAll "content/**"
        let indexCtx =
              listField "posts" postCtx (recentFirst $ newContent) `mappend`
              constField "title" "Home" `mappend`
              defaultContext
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d" `mappend` defaultContext

-- https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
pandocCompilerWithToc =
  pandocCompilerWith
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions
       { writerNumberSections = False
       , writerTableOfContents = True
       , writerTOCDepth = 3
       , writerTemplate =
           Just
             "<nav id=\"TableOfContents\">$toc$</nav><div class=\"content\">$body$</div>"
       })

pandocCompilerWithoutToc =
  pandocCompilerWith
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions
       { writerNumberSections = False
       , writerTableOfContents = False
       , writerTOCDepth = 3
       , writerTemplate = Just "<div class=\"content\">$body$</div>"
       })
