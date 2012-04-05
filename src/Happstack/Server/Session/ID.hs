{-# LANGUAGE FlexibleContexts #-}

module Happstack.Server.Session.ID where

import Control.Applicative (Alternative, optional)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.UUID           (UUID, toString, fromString)
import Happstack.Server    (CookieLife(MaxAge), FilterMonad, Response, HasRqData, mkCookie, addCookie, lookCookieValue)
import System.Random       (randomIO)

newtype SessionID = SessionID UUID

getSessionID :: ( Alternative m
                , FilterMonad Response m
                , MonadIO m
                , HasRqData m
                ) => m SessionID
getSessionID = getSessionID' "Happstack.SessionID" $ MaxAge maxBound

getSessionID' :: ( Alternative m
                 , FilterMonad Response m
                 , MonadIO m
                 , HasRqData m
                 ) => String -> CookieLife -> m SessionID
getSessionID' name age =
    optional (lookCookieValue name) >>= maybe newSession oldSession
  where
    oldSession = maybe newSession (return . SessionID) . fromString
    newSession = do uuid <- liftIO randomIO
                    addCookie age $ mkCookie name $ toString uuid
                    return $ SessionID uuid
