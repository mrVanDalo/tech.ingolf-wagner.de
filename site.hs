--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Data.Maybe (isNothing, isJust)
import Hakyll
import Text.Pandoc.Options

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith myConfiguration $
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
        loadAndApplyTemplate "templates/layout.html" (createDefaultIndex "articles")>>=
        relativizeUrls
    match "slides.markdown" $ do
      route $ setExtension "html"
      compile $ do
        newContent <- loadAll "slides/**"
        let indexCtx =
              listField "posts" postCtx (recentFirst $ newContent) `mappend`
              createDefaultIndex "slides"
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls
    match "articles.markdown" $ do
      route $ setExtension "html"
      compile $ do
        newContent <- loadAll "content/**"
        let indexCtx =
              listField "posts" postCtx (recentFirst $ newContent) `mappend`
              createDefaultIndex "articles"
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls
    match "about.markdown" $ do
      route $ setExtension "html"
      compile $ do
        let indexCtx =
              createDefaultIndex "about"
        pandocCompilerWithoutToc >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/layout.html" indexCtx >>=
          relativizeUrls
    match "index.markdown" $ do
      route $ setExtension "html"
      compile $ do
        newContent <- loadAll "content/**"
        let indexCtx =
              listField "posts" postCtx (recentFirst $ newContent) `mappend`
              createDefaultIndex "main"
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


createDefaultIndex :: String -> Context String
createDefaultIndex groupName =
  let navigationItems =
        [ ("/", "main")
        , ("/articles.html", "articles")
        , ("/slides.html", "slides")
        , ("/about.html", "about")
        ]
      listItem (path, label) active =
        let listPart content =
              if active
                then "<li class=\"active\">" ++ content ++ "<li>"
                else "<li>" ++ content ++ "</li>"
            linkPart url content =
              "<a href=\"" ++ url ++ "\">" ++ content ++ "</a>"
         in listPart (linkPart path label)
      navigation =
        foldl
          (\result item@(_, name) -> result ++ listItem item (name == groupName))
          ""
          navigationItems
   in constField "navigation" navigation `mappend` defaultContext

myConfiguration :: Configuration
myConfiguration =
  defaultConfiguration
    { deployCommand =
        "rsync -avz --delete _site/ root@sputnik.private:/srv/www/tech/"
    }
