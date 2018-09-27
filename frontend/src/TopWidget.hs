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

type MainAPI =
  "messages"  :> Capture "last" Int :> Get '[JSON] RecieveMessages
  :<|> "message" :> ReqBody '[JSON] PostMessage
    :> Post '[JSON] RecieveMessages
  :<|> "auth" :> "logout" :> Post '[JSON] ()

type LoginAPI =
  "auth" :> "login" :> ReqBody '[JSON] LoginData :> Post '[PlainText] Text
  :<|> "register" :> ReqBody '[JSON] LoginData :> Post '[PlainText] Text

type User = Text
type Message = Text

data MessageObject = MessageObject User Message
  deriving (Generic, Show)

-- Send the message along with the last synced message id (length actually)
data PostMessage = PostMessage Message Int
  deriving (Generic, Show)

data RecieveMessages = RecieveMessages [MessageObject] Int
  deriving (Generic, Show)

data LoginData = LoginData Text Text
  deriving (Generic, Show)

instance ToJSON MessageObject where
  toJSON (MessageObject user msg) =
    object ["user" .= user, "msg" .= msg]

instance FromJSON MessageObject where
  parseJSON (Object v) = MessageObject
        <$> v .: "user"
        <*> v .: "msg"

instance ToJSON PostMessage where
  toJSON (PostMessage msg i) =
    object ["id" .= i, "msg" .= msg]

instance FromJSON PostMessage where
  parseJSON (Object v) = PostMessage
        <$> v .: "msg"
        <*> v .: "id"

instance ToJSON RecieveMessages where
  toJSON (RecieveMessages msgs i) =
    object ["msgs" .= msgs, "id" .= i]

instance FromJSON RecieveMessages where
  parseJSON (Object v) = RecieveMessages
        <$> v .: "msgs"
        <*> v .: "id"

instance ToJSON LoginData where
  toJSON (LoginData u p) =
    object ["name" .= u, "pass" .= p]

instance FromJSON LoginData where
  parseJSON (Object v) = LoginData
        <$> v .: "name"
        <*> v .: "pass"

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
  regev <- button "Register"

  let
    doLogin :<|> doRegister = client (Proxy :: Proxy LoginAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "http://localhost:3000/"))
    loginData = LoginData <$> (value un) <*> (value pw)
  regRes <- doRegister (Right <$> loginData) regev
  let successEv = fmap (\r -> Nothing /= reqSuccess r) regRes

  widgetHold (return ()) $
    ffor successEv (\b -> if b
      then (text "Success")
      else (text "Try again"))

  fmapMaybe reqSuccess <$> doLogin (Right <$> loginData) ev

runChatWindow :: forall t m . MonadWidget t m => Text -> m (Event t ())
runChatWindow authTok = do

  -- servant-reflex computes FRP functions for each MainAPI endpoint
  let (getMessages :<|> postMessage :<|> doLogout) =
        clientWithOpts (Proxy :: Proxy MainAPI)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BasePath "http://localhost:3000/"))
          tweakRequest

      tweakRequest = ClientOptions $ \r -> do
        putStrLn ("Got req: " ++ show r)
        return $ r
          & xhrRequest_config . xhrRequestConfig_headers
            %~ Map.insert "Authorization" ("Bearer " <> authTok)

  logoutEv <- button "Logout"
  doneLogout <- doLogout logoutEv
  refEv <- button "refresh"

  rec
    lastMsgIdDyn <- divClass "" $ do
      text "messages"

      rsp <- getMessages (Right <$> lastMsgIdDyn) refEv

      let (msgEv, msgIdEv) = splitE $ (\(RecieveMessages msgs i) -> (msgs,i)) <$>
            (fmapMaybe reqSuccess rsp)
      m <- foldDyn (++) [] msgEv
      display m
      m2 <- holdDyn 0  msgIdEv
      display m2
      return m2

    divClass "" $ do
      ti <- textInput def
      ev <- button "Post"
      postMessage ((\m i -> Right $ PostMessage  m i)
                   <$> value ti <*> lastMsgIdDyn) ev

  return (() <$ doneLogout)
