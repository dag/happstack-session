{-# LANGUAGE FlexibleContexts #-}

module Happstack.Server.Session.ID where

import Control.Applicative (Alternative, optional)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.UUID           (UUID, toString, fromString)
import Happstack.Server    (CookieLife(MaxAge), FilterMonad, Response, HasRqData, mkCookie, addCookie, lookCookieValue)
import System.Random       (randomIO)

cookieName :: String
cookieName = "Happstack.SessionID"

newtype SessionID = SessionID UUID

lookSession :: ( Alternative m
               , FilterMonad Response m
               , MonadIO m
               , HasRqData m
               ) => m SessionID
lookSession =
    optional (lookCookieValue cookieName) >>= maybe newSession oldSession
  where
    oldSession = maybe newSession (return . SessionID) . fromString
    newSession = do uuid <- liftIO randomIO
                    addCookie (MaxAge maxBound) $
                        mkCookie cookieName $ toString uuid
                    return $ SessionID uuid
