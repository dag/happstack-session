{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Happstack.Server.Session.ID
  ( SessionID
  , getSessionID
  , getSessionID'
  ) where

import Control.Applicative (Alternative, optional)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Data           (Data, Typeable)
import Data.SafeCopy       (SafeCopy(putCopy, getCopy), contain, safePut, safeGet)
import Data.UUID           (UUID, toString, fromString, toWords, fromWords)
import Happstack.Server    (CookieLife(MaxAge), FilterMonad, Response, HasRqData, mkCookie, addCookie, lookCookieValue)
import System.Random       (Random, randomIO)

newtype SessionID = SessionID UUID
                    deriving (Eq, Ord, Data, Typeable, Random)

instance Show SessionID where
  show (SessionID uuid) = "SessionID{" ++ show uuid ++ "}"

instance SafeCopy SessionID where
  putCopy (SessionID uuid) = contain . safePut . toWords $ uuid
  getCopy                  = contain $ do (a,b,c,d) <- safeGet
                                          return $ SessionID $ fromWords a b c d

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
    optional (lookCookieValue name) >>= maybe new old
  where
    old = maybe new (return . SessionID) . fromString
    new = do uuid <- liftIO randomIO
             addCookie age $ mkCookie name $ toString uuid
             return $ SessionID uuid
