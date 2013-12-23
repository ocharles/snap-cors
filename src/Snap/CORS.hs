{-# LANGUAGE OverloadedStrings #-}
-- | Add <http://www.w3.org/TR/cors/ CORS> (cross-origin resource sharing)
-- headers to a Snap application. CORS headers can be added either conditionally
-- or unconditionally to the entire site, or you can apply CORS headers to a
-- single route.
module Snap.CORS
  ( -- * Wrappers
    wrapCORS
  , wrapCORSWithOptions

  -- * Applying CORS to a specific response
  , applyCORS

    -- * Option Specification
  , CORSOptions(..)
  , defaultOptions

    -- ** Origin lists
  , OriginList(..)
  , OriginSet, mkOriginSet, origins
                            
    -- * Internals
  , HashableURI(..)
  ) where

import Control.Applicative
import Control.Monad (guard, mzero, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.CaseInsensitive (CI)
import Data.Hashable (Hashable(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.URI (URI (..), URIAuth (..),  parseURI)

import qualified Data.ByteString.Char8 as Char8
import qualified Data.CaseInsensitive as CI
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Snap

-- | A set of origins. RFC 6454 specifies that origins are a scheme, host and
-- port, so the 'OriginSet' wrapper around a 'HashSet.HashSet' ensures that each
-- 'URI' constists of nothing more than this.
newtype OriginSet = OriginSet { origins :: HashSet.HashSet HashableURI }

-- | Used to specify the contents of the @Access-Control-Allow-Origin@ header.
data OriginList
  = Everywhere
  -- ^ Allow any origin to access this resource. Corresponds to
  -- @Access-Control-Allow-Origin: *@ 
  | Nowhere
  -- ^ Do not allow cross-origin requests    
  | Origins OriginSet
  -- ^ Allow cross-origin requests from these origins.

-- | Specify the options to use when building CORS headers for a response. Most
-- of these options are 'Snap.Handler' actions to allow you to conditionally
-- determine the setting of each header.
data CORSOptions m = CORSOptions
  { corsAllowOrigin :: m OriginList
  -- ^ Which origins are allowed to make cross-origin requests.

  , corsAllowCredentials :: m Bool
  -- ^ Whether or not to allow exposing the response when the omit credentials
  -- flag is unset.

  , corsExposeHeaders :: m (HashSet.HashSet (CI Char8.ByteString))
  -- ^ A list of headers that are exposed to clients. This allows clients to
  -- read the values of these headers, if the response includes them.
  }

-- | Liberal default options. Specifies that:
--
-- * All origins may make cross-origin requests
-- * @allow-credentials@ is true.
-- * No extra headers beyond simple headers are exposed
--
-- All options are determined unconditionally.
defaultOptions :: Monad m => CORSOptions m
defaultOptions = CORSOptions
  { corsAllowOrigin = return Everywhere
  , corsAllowCredentials = return True
  , corsExposeHeaders = return HashSet.empty
  }

-- | Apply CORS for every request, unconditionally.
--
-- 'wrapCors' ≡ 'wrapCORSWithOptions' 'defaultOptions'
wrapCORS :: Snap.Initializer b v ()
wrapCORS = wrapCORSWithOptions defaultOptions

-- | Initialize CORS for all requests with specific options.
wrapCORSWithOptions :: CORSOptions (Snap.Handler b v) -> Snap.Initializer b v ()
wrapCORSWithOptions options = Snap.wrapSite (applyCORS options >>)

-- | Apply CORS headers to a specific request. This is useful if you only have
-- a single action that needs CORS headers, and you don't want to pay for
-- conditional checks on every request.
applyCORS :: Snap.MonadSnap m => CORSOptions m -> m ()
applyCORS options = void $ runMaybeT $ do
  origin <- MaybeT $ Snap.getsRequest (Snap.getHeader "Origin")
  originUri <- MaybeT $ pure $
    fmap simplifyURI $ parseURI $ Text.unpack $ decodeUtf8 origin

  originList <- lift $ corsAllowOrigin options

  case originList of
    Everywhere -> return ()
    Nowhere -> mzero
    (Origins (OriginSet xs)) ->
      guard (HashableURI originUri `HashSet.member` xs)

  lift $ do
    exposeHeaders <- corsExposeHeaders options

    addHeader "Access-Control-Allow-Origin"
              (encodeUtf8 $ Text.pack $ show originUri)
    allowCredentials <- corsAllowCredentials options
    when (allowCredentials) $
      addHeader "Access-Control-Allow-Credentials" "true"

    when (not $ HashSet.null exposeHeaders) $
      addHeader "Access-Control-Expose-Headers" $
        Char8.intercalate ", " (map CI.original $ HashSet.toList exposeHeaders)

 where
  addHeader k v = Snap.modifyResponse (Snap.addHeader k v)

mkOriginSet :: [URI] -> OriginSet
mkOriginSet = OriginSet . HashSet.fromList . map (HashableURI . simplifyURI)

simplifyURI :: URI -> URI
simplifyURI uri = uri { uriAuthority = fmap simplifyURIAuth (uriAuthority uri)
                       , uriPath = ""
                       , uriQuery = ""
                       , uriFragment = ""
                       }
 where simplifyURIAuth auth = auth { uriUserInfo = "" }

-- | A @newtype@ over 'URI' with a 'Hashable' instance.
newtype HashableURI = HashableURI URI
  deriving (Eq, Show)

instance Hashable HashableURI where
  hashWithSalt s (HashableURI (URI scheme authority path query fragment)) =
    s `hashWithSalt`
    scheme `hashWithSalt`
    fmap hashAuthority authority `hashWithSalt`
    path `hashWithSalt`
    query `hashWithSalt`
    fragment

   where
    hashAuthority (URIAuth userInfo regName port) =
          s `hashWithSalt`
          userInfo `hashWithSalt`
          regName `hashWithSalt`
          port
