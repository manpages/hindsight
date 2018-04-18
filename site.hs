{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Data.Monoid (mappend, mconcat)
import           Data.Binary (Binary)
import           Data.Set (insert)
import           Data.Typeable
import           Hakyll
import           Hakyll.Core.Identifier (fromFilePath)
import           Text.Pandoc.Options
import           Control.Monad (forM_)
import           System.IO (hSetEncoding, utf8, stdin, stdout, stderr)
import qualified GHC.IO.Encoding as E

--------------------------------------------------------------------------------
siteCat :: [String]
siteCat = [ "posts" ]

{--
matcher :: String -> Rules () -> Rules ()
matcher "drafts" = match (fromGlob "drafts/*")
matcher x        = match (fromGlob $ "posts/" ++ x ++ "/*")

loader  :: (Binary a, Typeable a) => String -> Compiler [Item a]
loader "drafts"  = loadAll (fromGlob "drafts/*")
loader x         = loadAll (fromGlob $ "posts/" ++ x ++ "/*")
--}

main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  E.setFileSystemEncoding E.utf8
  E.setForeignEncoding E.utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]

  hakyll $ do

    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match (fromList ["plan.html", "favicon.ico"]) $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["me.html", "index.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
          route $ setExtension "html"
          compile $ pandocMathCompiler
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField  "posts" postCtx (return posts) `mappend`
                    constField "title"    "Everything"        `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          renderRss feedConfig feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedCtx =
    bodyField "description" `mappend`
    postCtx

title = id

feedConfig = FeedConfiguration {
     feedTitle       = "Jоnn Hindsights"
    ,feedDescription = "A personal blog about functional programming, information security, operating systems, literature and other (less interesting) topcis."
    ,feedAuthorName  = "Jonn Mostovoy"
    ,feedAuthorEmail = "jm at this domain"
    ,feedRoot        = "http://memorici.de"
}

--------------------------------------------------------------------------------

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                        Ext_latex_macros]
      defaultExtensions = writerExtensions defaultHakyllWriterOptions
      newExtensions = foldr Data.Set.insert defaultExtensions mathExtensions
      writerOptions = defaultHakyllWriterOptions {
                        writerExtensions = newExtensions,
                        writerHTMLMathMethod = MathJax ""
                      }
  in pandocCompilerWith
       (defaultHakyllReaderOptions {readerSmart = False})
       writerOptions
