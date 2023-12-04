import Test.Hspec
import Database.HDBC.Sqlite3 (connectSqlite3)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

-- Import your module with the methods
import Repository

main :: IO ()
main = hspec $ do
  describe "Repository Operations" $ do
    it "should be able to initialize and close the repository" $ do
      repo <- initializeRepository
      closeRepository repo

    it "should be able to insert and retrieve a post" $ do
      repo <- initializeRepository
      timestamp <- getCurrentTime
      let testPost = Post 1 "Test Title" "Test Content" timestamp "test-link"
      postId <- insertPost repo testPost

      retrievedPost <- getPostById repo postId
      retrievedPost `shouldBe` Just testPost

      closeRepository repo

  describe "Database Queries" $ do
    it "should build a valid SELECT query for all posts" $ do
      let query = selectPosts ""
      show query `shouldBe` "SELECT id, title, content, timestamp, link FROM posts "

    it "should build a valid SELECT query for public posts" $ do
      let query = selectPosts "WHERE private = 0"
      show query `shouldBe` "SELECT id, title, content, timestamp, link FROM posts WHERE private = 0"

    it "should build a valid UPDATE query to set a post as private" $ do
      let query = setPostPrivate " = 1 WHERE id = ?"
      show query `shouldBe` "UPDATE posts SET private = 1 WHERE id = ?"

    it "should build a valid UPDATE query to set a post as public" $ do
      let query = setPostPrivate "= 0 WHERE id = ?"
      show query `shouldBe` "UPDATE posts SET private = 0 WHERE id = ?"