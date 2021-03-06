import Network.Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import Data.Monoid
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)

-- | write TMVar even if it's already full.
forcePutTMVar :: TMVar a -> a -> STM ()
forcePutTMVar var a = do
    tryTakeTMVar var
    putTMVar var a

type SimpleState = L.ByteString

-- handle http requests
app :: TMVar SimpleState -> Application
app state req = case pathInfo req of
    [] -> do
        d <- liftIO $ atomically $ readTMVar state
        return $ response d
    _ -> return response404
  where
    response = ResponseBuilder status200 [] . B.fromLazyByteString
    response404 = ResponseBuilder status404 [] mempty

-- update the cache periodically.
cacher :: String -> TMVar SimpleState -> Int -> IO ()
cacher url state interval =
    forever $
        try (HTTP.simpleHttp url) >>= either onError onSuccess
  where
    onSuccess rsp = do
        atomically $ forcePutTMVar state rsp
        threadDelay interval
    onError exc = print (exc::SomeException)

main :: IO ()
main = do
    let port = 3000
    state <- newEmptyTMVarIO
    _ <- forkIO (cacher "http://10.10.10.3:9002/" state 1000000)
    putStrLn $ "http://localhost:"++show port
    run port (app state)
