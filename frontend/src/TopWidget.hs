{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TopWidget where

import Data.Aeson
import Protolude hiding ((&))
import Control.Lens hiding ((.=))
import Reflex.Dom
import Servant.API
import Servant.Reflex
import Data.Proxy
import Data.Text
import qualified Data.Map as Map

newtype UserId = UserId Int
  deriving (Generic, Show)

data MessageObject = MessageObject UserId Text
  deriving (Generic, Show)

data LoginData = LoginData Text Text
  deriving (Generic, Show)

instance ToJSON MessageObject where
  toJSON (MessageObject (UserId uid) msg) =
    object ["userid" .= uid, "msg" .= msg]

instance FromJSON MessageObject where
  parseJSON (Object v) = MessageObject
        <$> (UserId <$> v .: "userid")
        <*> v .: "msg"

instance ToJSON LoginData where
  toJSON (LoginData u p) =
    object ["name" .= u, "pass" .= p]

instance FromJSON LoginData where
  parseJSON (Object v) = LoginData
        <$> v .: "name"
        <*> v .: "pass"

type MainAPI =
  "messages"  :> Get '[JSON] [MessageObject]
  :<|> "message" :> ReqBody '[JSON] Text
    :> Post '[JSON] Int
  :<|> "auth" :> "logout" :> Post '[JSON] ()

type LoginAPI =
  "auth" :> "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] Text

topWidget :: MonadWidget t m => m ()
topWidget = do
  rec
    lEv <- loginWindow

    widgetHold (return (never))
      (runChatWindow <$> lEv)
  return ()

loginWindow :: forall t m . MonadWidget t m => m (Event t Text)
loginWindow = do
  un <- textInput def
  pw <- textInput def
  ev <- button "Login"

  let
    doLogin = client (Proxy :: Proxy LoginAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "/"))
    loginData = LoginData <$> (value un) <*> (value pw)
  fmapMaybe reqSuccess <$> doLogin (Right <$> loginData) ev

runChatWindow :: forall t m . MonadWidget t m => Text -> m (Event t ())
runChatWindow authTok = do

  -- servant-reflex computes FRP functions for each MainAPI endpoint
  let (getMessages :<|> postMessage :<|> doLogout) =
        clientWithOpts (Proxy :: Proxy MainAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "/"))
          tweakRequest

      tweakRequest = ClientOptions $ \r -> do
        putStrLn ("Got req: " ++ show r)
        return $ r
          & xhrRequest_config . xhrRequestConfig_headers
            %~ Map.insert "Authorization" ("Bearer " <> authTok)

  logoutEv <- button "Logout"
  doneLogout <- doLogout logoutEv
  refEv <- button "refresh"

  divClass "" $ do
    text "messages"

    msgEv <- getMessages refEv
    m <- foldDyn (++) [] (fmapMaybe reqSuccess msgEv)
    display m

  divClass "" $ do
    ti <- textInput def
    ev <- button "Post"
    postMessage (Right <$> value ti) ev

  return (() <$ doneLogout)


-- -- No need to write these functions. servant-reflex creates them for you!
--    getint :: MonadWidget t m
--           => Event t ()  -- ^ Trigger the XHR Request
--           -> m (Event t (ReqResult () Int)) -- ^ Consume the answer

--    sayhi :: MonadWidget t m
--          => Dynamic t (QParam Text) 
--             -- ^ One input parameter - the 'name', wrapped in 'QParam'
--          -> Dynamic t [Text]
--             -- ^ Another input: list of preferred greetings
--          -> Dynamic t Bool
--             -- ^ Flag for capitalizing the response
--          -> Event t ()
--             -- ^ Trigger the XHR Request
--          -> m (Event t (ReqResult () Text))

--    doubleit :: MonadWidget t m
--             => Dynamic t (Either Text Double)
--             -> Event t ()
--             -> m (Event t (ReqResult () Double))
