{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Useful utilities for using Lucid with Yesod.
-- Taken almost verbatim from https://github.com/haskell-infra/hl; the licence
-- is reproduced below:
--
-- Copyright (c) 2014, haskell-lang
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of haskell-lang nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module YesodExtras.Lucid
  ( module YesodExtras.Lucid
  , module Lucid
  )
  where

import Lucid
import ClassyPrelude.Yesod hiding (Html)
import qualified Blaze.ByteString.Builder as Blaze

data LucidReader a
  = LucidReader { rdrCurrentRoute :: Maybe (Route a)
                , rdrUrlRender    :: Route a -> Text
                }

-- | A lucid generator.
type FromLucid a = HtmlT (Reader (LucidReader a)) ()

newtype RenderedHtml = RenderedHtml Blaze.Builder

-- | Output some lucid, passes a URL renderer to the continuation.
lucid :: MonadHandler m => FromLucid (HandlerSite m) -> m RenderedHtml
lucid act =
  do render <- getUrlRender
     route <- getCurrentRoute
     return (RenderedHtml (runReader (execHtmlT act) (LucidReader route render)))

  where
  runReader r a = runIdentity (runReaderT r a)

getCurrentRoute' :: (Functor m, MonadReader (LucidReader a) m) => m (Maybe (Route a))
getCurrentRoute' = rdrCurrentRoute <$> ask

getUrlRender' :: (Functor m, MonadReader (LucidReader a) m) => m (Route a -> Text)
getUrlRender' = rdrUrlRender <$> ask

instance ToTypedContent RenderedHtml where
  toTypedContent html =
    TypedContent (getContentType (Just html))
                 (toContent html)

instance ToContent RenderedHtml where
  toContent (RenderedHtml b) =
    ContentBuilder b Nothing

instance HasContentType RenderedHtml where
  getContentType _ = "text/html"
