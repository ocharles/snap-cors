{-# LANGUAGE OverloadedStrings #-}
module Snap.CORS
  ( -- * Wrappers
    wrapCORS
  , wrapCORSWithOptions

  -- * Applying CORS to a specific response
  , applyCORS

    -- * Option Specification
  , CORSOptions(..)
  , defaultOptions
    
  , OriginList(..)
  ) where

import Control.Applicative
import Control.Monad (guard, mzero, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Hashable (Hashable(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.URI (URI (..), URIAuth (..),  parseURI)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Snap

newtype OriginSet = OriginSet (HashSet.HashSet HashableURI)

-- | Used to specify the contents of the @Access-Control-Allow-Origin@ header.
data OriginList
  = Everywhere
  -- ^ Allow any origin to access this resource. Corresponds to
  -- @Access-Control-Allow-Origin: *@ 
  | Nowhere
  -- ^ Do not allow cross-origin requests    
  | Origins OriginSet
  -- ^ Allow cross-origin requests from these origins. RFC 6454 specifies that
  -- origins are a scheme, host and port - so it follows that if you specify any
  -- other parts of a 'URI' this Snaplet will discard this information to meet
  -- the specification.

-- | Specify the options to use when building CORS headers for a response. Most
-- of these options are 'Snap.Handler' actions to allow you to conditionally
-- determine the setting of each header.
data CORSOptions m = CORSOptions
  { corsAllowOrigin :: m OriginList
  -- ^ Which origins are allowed to make cross-origin requests.

  , corsAllowCredentials :: m Bool
  -- ^ Whether or not to allow exposing the response when the omit credentials
  -- flag is unset.  
  }

-- | Liberal default options. Specifies that all origins may make cross-origin
-- requests, allow-credentials is true. Headers are determined unconditionally.
defaultOptions :: Monad m => CORSOptions m
defaultOptions = CORSOptions
  { corsAllowOrigin = return Everywhere
  , corsAllowCredentials = return True
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
    addHeader "Access-Control-Allow-Origin"
              (encodeUtf8 $ Text.pack $ show originUri)
    allowCredentials <- corsAllowCredentials options
    when (allowCredentials) $
      addHeader "Access-Control-Allow-Credentials" "true"

 where
  addHeader k v = Snap.modifyResponse (Snap.addHeader k v)

encodeOriginList :: OriginList -> BS.ByteString
encodeOriginList Everywhere = Char8.pack "*"
encodeOriginList Nowhere = Char8.pack "null"
encodeOriginList (Origins (OriginSet origins)) =
  if HashSet.null origins
    then Char8.pack "null"
    else Char8.intercalate
           (Char8.singleton ' ')
           (map (Char8.pack . show) (HashSet.toList origins))

originSet :: [URI] -> OriginSet
originSet = OriginSet . HashSet.fromList . map (HashableURI . simplifyURI)

simplifyURI :: URI -> URI
simplifyURI uri = uri { uriAuthority = fmap simplifyURIAuth (uriAuthority uri)
                       , uriPath = ""
                       , uriQuery = ""
                       , uriFragment = ""
                       }
 where simplifyURIAuth auth = auth { uriUserInfo = "" }

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
