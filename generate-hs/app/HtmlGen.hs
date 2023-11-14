{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}


module HtmlGen(
  patchIndexWith
) where

import Text.Blaze.Html5 as H hiding (title)
import Text.Blaze.Html5.Attributes as A hiding (title, content)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Time.Clock
import Data.Function
import Data.Time (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Markdown (markdown, def)

import Structure (Post(..))

(|>) :: a -> (a -> b) -> b
(|>) = (&)

htmlclass :: String -> Attribute
htmlclass str = A.class_ (toValue str)

formatUTCTime :: Maybe UTCTime -> Maybe String
formatUTCTime (Just time) = Just $ formatTime defaultTimeLocale "%d-%m-%Y" time
formatUTCTime _ = Nothing

markdownToHtml :: TL.Text -> Html
markdownToHtml = preEscapedToHtml . markdown def

-- Function to generate HTML for a single post
-- Convert a Post to HTML
postToHtml :: Post -> Html
postToHtml post =
  H.div ! htmlclass "card" $
    H.div ! htmlclass "info" $
      do
        H.div ! htmlclass "card-title" $
          do
            H.h3 ! htmlclass "title" $ toHtml $ title post
            H.p ! htmlclass "timestamp" $ maybe mempty toHtml (formatUTCTime $ timestamp post)
        toHtml (markdownToHtml $ TL.pack $ content post) ! htmlclass "content"
        H.a ! htmlclass "link" ! A.style "width: 3.5rem;" ! A.href "http://example.com" $ "Red pill"

-- Function to generate HTML for a list of posts and wrap them in a div with id "posts"
postsToHtml :: [Post] -> Html
postsToHtml = foldMap postToHtml

patchIndexWith :: [Post] -> IO ()
patchIndexWith posts = do
  index <- readFile "index.html"
  let insertionPortion = T.replace "<!-- placeholder -->" ("<div class=\"timeline\"><div class=\"outer\">" <> T.pack (renderHtml (postsToHtml posts) <> "</div></div>")) (T.pack index)
  (putStrLn . T.unpack) insertionPortion
  writeFile "index-u.html" (T.unpack insertionPortion)

-- Example usage
main :: IO ()
main = {-
  ([     Post 1 "Title 1" "Content 1" (Just $ read "2023-01-01 12:00:00 UTC"),
        Post 2 "Title 2" "Content 2" Nothing,
        Post 3 "Title 3" "Content 3" (Just $ read "2023-01-03 15:30:00 UTC")
  ] |> postsToHtml
  |> renderHtml
  |> putStrLn) >> -} ([     Post 1 "Title 1" "Content 1 generated  Long style content who even has to say that much like wtf dont bother me with this waste of sapce" (Just $ read "2023-01-01 12:00:00 UTC") Nothing,
        Post 2 "Title 2" "Content 2 generated" Nothing Nothing,
        Post 3 "Title 3" "Content 3 generated" (Just $ read "2023-01-03 15:30:00 UTC") Nothing
  ] |> patchIndexWith
  )
