{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

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

import GHCJS.DOM as X
import qualified Language.Javascript.JSaddle.Types as X
import GHCJS.DOM.Document
import GHCJS.DOM.Element


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
  let
    setCss = X.liftJSM $ do
      doc <- currentDocumentUnchecked
      headElement <- getHeadUnchecked doc
      setInnerHTML headElement $
        ("<link rel=\"stylesheet\" href=\"https://bulma.io/css/bulma-docs.min.css?v=201809250813\">" :: Text)

  setCss
  pb <- getPostBuild
  rec
    lEv <- widgetHoldWithRemoveAfterEvent (loginWindow <$ leftmost [pb, lo])

    lo <- widgetHoldWithRemoveAfterEvent (runChatWindow <$> lEv)
  return ()

loginWindow :: forall t m . MonadWidget t m => m (Event t Text)
loginWindow = do
  elClass "section" "hero is-primary is-medium" $
    divClass "hero-body" $ do
      divClass "container has-text-centered" $ do
        elClass "h1" "title" $ do
          text "Please login to enter chat room"

        un <- elClass "h1" "title" $ do
          textInput def
        pw <- elClass "h2" "subtitle" $ do
          textInput def

        let
          doLogin :<|> doRegister = client (Proxy :: Proxy LoginAPI)
                (Proxy :: Proxy m)
                (Proxy :: Proxy ())
                (constDyn (BasePath "http://localhost:3000/"))
          loginData = LoginData <$> (value un) <*> (value pw)

        loginEv <- elClass "h2" "subtitle" $ do
          ev <- button "Login"
          fmapMaybe reqSuccess <$> doLogin (Right <$> loginData) ev

        elClass "h2" "subtitle" $ do
          regev <- button "Register"
          regRes <- doRegister (Right <$> loginData) regev
          let successEv = fmap (\r -> Nothing /= reqSuccess r) regRes

          widgetHold (return ()) $
            ffor successEv (\b -> if b
              then (text "Success")
              else (text "Try again"))

        return (loginEv)


runChatWindow :: forall t m . MonadWidget t m => Text -> m (Event t ())
runChatWindow authTok = divClass "container" $ do

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

  logoutEv <- navBar
  doneLogout <-
    doLogout logoutEv

  refEv <- tickLossyFromPostBuildTime 20

  divClass "" $ do
    let attr = ("class" =: "section") <>
          ("style" =: "height: 80vh; overflow: auto")
    rec
      lastMsgIdDyn <- elAttr "div" attr $ do
        rsp <- getMessages (Right <$> lastMsgIdDyn) (() <$ refEv)

        let (msgEv, msgIdEv) = splitE $ (\(RecieveMessages msgs i) -> (msgs,i)) <$>
              (fmapMaybe reqSuccess $ leftmost [rsp, updMsgs])
        m <- foldDyn (flip (++)) [] msgEv
        simpleList m (\a -> dyn (showMsg <$> a))
        holdDyn 0  msgIdEv

      updMsgs <- divClass "section" $ do
        rec
          ti <- textInput $ def
            & textInputConfig_setValue .~ ("" <$ ev)
            & textInputConfig_attributes .~ (constDyn ("class" =: "input"))
          let ev = keypress Enter ti
        postMessage ((\m i -> Right $ PostMessage  m i)
                     <$> value ti <*> lastMsgIdDyn) ev
    return ()

  return (() <$ doneLogout)

navBar :: (MonadWidget t m, _) => m (Event t ())
navBar = do
  elClass "nav" "navbar is-transparent" $ do
    divClass "navbar-brand" $ do
      let
        attr1 = ("class" =: "navbar-item")
      elAttr "a" attr1 $ do
        text "Demo Chat App"
        return ()
      let
        attr3 = ("class" =: "navbar-burger burger") <>
                ("data-target" =: "navbarExampleTransparentExample")
      elAttr "div" attr3 $ do
        el "span" $ return ()
        el "span" $ return ()
        el "span" $ return ()
        return ()
      return ()
    let
      attr4 = ("class" =: "navbar-menu") <>
              ("id" =: "navbarExampleTransparentExample")
    elAttr "div" attr4 $ do
      divClass "navbar-end" $ do
        divClass "navbar-item" $ do
          divClass "field is-grouped" $ do
            elClass "p" "control" $ do
              let
                attr15 = ("class" =: "button is-primary")
              (e,_) <- elAttr' "a" attr15 $ do
                elClass "span" "" $ do
                  text "Logout"
              return (domEvent Click e)

showMsg :: MonadWidget t m => MessageObject -> m ()
showMsg (MessageObject user msg) = do
  elClass "article" "media" $ do
    divClass "media-content" $ do
      divClass "content" $ do
        el "p" $ do
          el "strong" $ do
            text user
          el "br" $ return ()
          text msg

-- widgetHoldWithRemoveAfterEvent
--   :: (MonadFix m,
--        MonadHold t m,
--        DomBuilder t m)
--   => Event t (m (Event t a))
--   -> m (Event t a)
widgetHoldWithRemoveAfterEvent wEv = do
  let
    f1 w = do
      rec
        let ev = (switch . current) evDyn
        evDyn <- widgetHold (w)
          (return never <$ ev)
      return ev
  evDyn <- widgetHold (return never)
    (f1 <$> wEv)
  return $ (switch . current) evDyn
