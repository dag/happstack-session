{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Happstack.Server.Session.Client
  ( ClientSession(empty)
  , getSession
  , putSession
  , getSession'
  , putSession'
  ) where

import Control.Applicative   (Alternative, optional)
import Control.Monad.Trans   (MonadIO, liftIO)
import Data.ByteString.Char8 (pack, unpack)
import Data.SafeCopy         (SafeCopy, safeGet, safePut)
import Data.Serialize        (runGet, runPut)
import Happstack.Server      (FilterMonad, Response, HasRqData, CookieLife(MaxAge), lookCookieValue, addCookie, mkCookie)
import Web.ClientSession     (Key, getDefaultKey, decrypt, encryptIO)

class SafeCopy a => ClientSession a where
  empty :: a

getSession :: ( Alternative m
              , FilterMonad Response m
              , MonadIO m
              , HasRqData m
              , ClientSession a
              ) => m a
getSession =
    do key <- liftIO getDefaultKey
       getSession' "Happstack.ClientSession" (MaxAge maxBound) key

putSession :: ( Alternative m
              , FilterMonad Response m
              , MonadIO m
              , HasRqData m
              , ClientSession a
              ) => a -> m ()
putSession value =
    do key <- liftIO getDefaultKey
       putSession' "Happstack.ClientSession" (MaxAge maxBound) key value

getSession' :: forall m a.
               ( Alternative m
               , FilterMonad Response m
               , MonadIO m
               , HasRqData m
               , ClientSession a
               ) => String -> CookieLife -> Key -> m a
getSession' name age key =
    optional (lookCookieValue name) >>= maybe new old
  where
    old = maybe new (either (const new) return . runGet safeGet) . decrypt key . pack
    new = do putSession' name age key (empty :: a)
             return empty

putSession' :: ( Alternative m
               , FilterMonad Response m
               , MonadIO m
               , HasRqData m
               , ClientSession a
               ) => String -> CookieLife -> Key -> a -> m ()
putSession' name age key value =
    do bytes <- liftIO $ encryptIO key (runPut $ safePut value)
       addCookie age $ mkCookie name $ unpack bytes
