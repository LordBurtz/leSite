module Main where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Time.Clock
import Data.Function
import Data.Time (formatTime, defaultTimeLocale)

(|>) = (&)

htmlclass :: String -> Attribute
htmlclass str = A.class_ (toValue str)

data Post = Post {
  postId :: Int,
  postTitle :: String,
  postContent :: String,
  postTimestamp :: Maybe UTCTime
} deriving (Show)

formatUTCTime :: Maybe UTCTime -> Maybe String
formatUTCTime (Just time) = Just $ formatTime defaultTimeLocale "%d-%m-%Y" time
formatUTCTime _ = Nothing

-- Function to generate HTML for a single post
-- Convert a Post to HTML
postToHtml :: Post -> Html
postToHtml post =
  H.div ! htmlclass "card" $
    H.div ! htmlclass "info" $
      do
        H.div ! htmlclass "card-title" $
          do
            H.h3 ! htmlclass "title" $ toHtml $ postTitle post
            H.p ! htmlclass "timestamp" $ maybe mempty toHtml (formatUTCTime $ postTimestamp post)
        H.p ! htmlclass "content" $ toHtml $ postContent post

-- Function to generate HTML for a list of posts and wrap them in a div with id "posts"
postsToHtml :: [Post] -> Html
postsToHtml = foldMap postToHtml

-- Example usage
main :: IO ()
main =
  [     Post 1 "Title 1" "Content 1" (Just $ read "2023-01-01 12:00:00 UTC"),
        Post 2 "Title 2" "Content 2" Nothing,
        Post 3 "Title 3" "Content 3" (Just $ read "2023-01-03 15:30:00 UTC")
  ] |> postsToHtml 
  |> renderHtml 
  |> putStrLn