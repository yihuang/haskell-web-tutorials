{-# LANGUAGE DeriveGeneric #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import GHC.Generics
import Data.Monoid
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import qualified Blaze.ByteString.Builder as B
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)

newtype SimpleState = SimpleState Int
    deriving (Generic)

instance ToJSON SimpleState
instance FromJSON SimpleState where
    parseJSON (Array vec) = return $ SimpleState $ V.length vec
    parseJSON o = typeMismatch "SimpleState" o

-- handle http requests
app :: MVar SimpleState -> Application
app state req = case pathInfo req of
    [] -> do
        d <- liftIO (takeMVar state)
        return $ response (encode d)
    _ -> return response404
  where
    response = ResponseBuilder status200 [] . B.fromLazyByteString
    response404 = ResponseBuilder status404 [] mempty

-- update the cache periodically.
cacher :: String -> MVar SimpleState -> Int -> IO ()
cacher url state interval = forever $ do
    ersp <- try (HTTP.simpleHttp url)
    case ersp of
        Right rsp -> maybe (putStrLn "decode json failed.")
                           (void . swapMVar state)
                           (decode rsp)
        Left err -> print (err::SomeException)
    threadDelay interval

main :: IO ()
main = do
    let port = 3000
    state <- newEmptyMVar :: IO (MVar SimpleState)
    _ <- forkIO (cacher "http://10.10.10.3:9002/" state 1000)
    putStrLn $ "http://localhost:"++show port
    run port (app state)
