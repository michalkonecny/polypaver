{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Hakyll
import Text.Pandoc
import Data.Monoid (mappend, mconcat)
import qualified Data.Map as M

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` defaultContext

bitCtx = postCtx

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

newsCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "PolyPaver | News"
  `mappend` defaultContext

tutorialsCtx posts =
  listField "tutorials" postCtx (return posts)
  `mappend` constField "title" "PolyPaver | Tutorials"
  `mappend` defaultContext

indexCtx posts bits =
  listField "posts" postCtx (return posts)
  `mappend` 
  constField "title" "PolyPaver | Home"
  `mappend`
  bitsContext bits
  `mappend` 
  defaultContext

bitsContext bits =
    mathCtx `mappend` 
    (mconcat $ map setBit bits) 
    where
    setBit (Item bitId bitBody) =
        constField bitName bitBody
        where
        bitName = drop (length ("bits/" :: String)) $ toFilePath bitId

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

static :: Rules ()
static = do
  match "fonts/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "img/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*" $ do
    route idRoute
    compile $ copyFileCompiler

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ setExtension "html"
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      bits <- recentFirst =<< loadAll "bits/*"
      (compiler shouldAddTOC)
        >>= loadAndApplyTemplate "templates/page.html"    postCtx
        >>= applyAsTemplate (indexCtx posts bits)
        >>= relativizeUrls
    where
    shouldAddTOC = True


newsPosts :: Rules ()
newsPosts = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $ (compiler shouldAddTOC)
      >>= loadAndApplyTemplate "templates/newsPost.html"    postCtx
      >>= relativizeUrls
    where
    shouldAddTOC = True

newsIndex :: Rules ()
newsIndex = do
  create ["news.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/news.html" (newsCtx posts)
        >>= relativizeUrls

tutorials :: Rules ()
tutorials = do
  match "tutorials/*" $ do
    route $ setExtension "html"
    compile $ (compiler shouldAddTOC)
      >>= loadAndApplyTemplate "templates/tutorial.html"    postCtx
      >>= relativizeUrls
    where
    shouldAddTOC = True

tutorialsIndex :: Rules ()
tutorialsIndex = do
  create ["tutorials.html"] $ do
    route idRoute
    compile $ do
      tutorials <- recentFirst =<< loadAll "tutorials/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/tutorials.html" (tutorialsCtx tutorials)
        >>= relativizeUrls

index :: Rules ()
index = do
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      bits <- recentFirst =<< loadAll "bits/*"
      getResourceBody
        >>= applyAsTemplate (indexCtx posts bits)
        >>= relativizeUrls

templates :: Rules ()
templates = 
    match "templates/*" $ compile templateCompiler

bits :: Rules ()
bits = 
    match "bits/*" $ 
    compile $ (compiler shouldAddTOC)
--      >>= loadAndApplyTemplate "templates/bit.html" bitCtx
      >>= relativizeUrls
    where
    shouldAddTOC = False

--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Bool -> Compiler (Item String)
compiler shouldAddTOC = pandocCompilerWith defaultHakyllReaderOptions (pandocOptions shouldAddTOC)

pandocOptions :: Bool -> WriterOptions
pandocOptions shouldAddTOC = 
    defaultHakyllWriterOptions
    { 
        writerSectionDivs = True,
        writerStandalone = True,
        writerTableOfContents = shouldAddTOC,
        writerTemplate = tocTemplate,
        writerHTMLMathMethod = MathJax ""
    }

tocTemplate =
    unlines $
    [ 
        "$if(toc)$",
        "<h3>Table of contents</h3>",
        "$toc$",
        "<hr class=\"divider\">",
        "$endif$",
        "$body$"
    ]
    

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  static
  pages
  newsPosts
  newsIndex
  tutorials
  tutorialsIndex
  index
  templates
  bits
