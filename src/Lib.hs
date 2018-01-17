{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch, catch, SomeException)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.Text (Text, pack)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets (ConnectionException)
import Network.WebSockets.Connection (Connection, sendTextData, sendClose, forkPingThread, receiveDataMessage )
import Servant
import Servant.API.WebSocket
import Servant.Swagger

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''User)

type API = SwaggerApi :<|> UserApi :<|> WebSocketApi

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = swaggerHandler :<|> userHandler :<|> streamData


type UserApi = "users" :> Get '[JSON] [User]


userHandler :: Handler [User]
userHandler = return users


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

type WebSocketApi = "stream" :> WebSocket


streamData :: forall m . (MonadCatch m, MonadIO m) => Connection -> m ()
streamData c = do
  liftIO $ putStrLn "new connection..."
  go `catch` closed
  where
    go = liftIO $ do
      forkPingThread c 10
      forM_ [1..] $ \i -> sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
    closed :: ConnectionException -> m ()
    closed _ = liftIO $ putStrLn "socket went away :'("
    panic :: SomeException -> m ()
    panic _ = liftIO $ putStrLn "something went wrong"


type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger


swaggerHandler :: Handler Swagger
swaggerHandler = return $ toSwagger (Proxy :: Proxy UserApi)


instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "This is some real Todo right here"
    & mapped.schema.example ?~ toJSON (User 1 "Isaac" "Newton" )
