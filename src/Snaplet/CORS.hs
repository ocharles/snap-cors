{-# LANGUAGE OverloadedStrings #-}
module Snaplet.CORS
  ( -- * Initializers
    initCORS
  , initCORSWithOptions

  -- * Applying CORS to a specific response
  , applyCORS

    -- * Option Specification
  , CORSOptions(..)
  , defaultOptions
    
  , OriginList(..)
  ) where

import Control.Applicative
import Control.Monad (when)
import Network.URI (URI)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Snap                                          

-- | Used to specify the contents of the @Access-Control-Allow-Origin@ header.
data OriginList
  = Everywhere
  -- ^ Allow any origin to access this resource. Corresponds to
  -- @Access-Control-Allow-Origin: *@ 
  | Nowhere
  -- ^ Do not allow cross-origin requests    
  | Origins [URI]
  -- ^ Allow cross-origin requests from these origins. RFC 6454 specifies that
  -- origins are a scheme, host and port - so it follows that if you specify any
  -- other parts of a 'URI' this Snaplet will discard this information to meet
  -- the specification.

data CORS = CORS

-- | Specify the options to use when building CORS headers for a response. Most
-- of these options are 'Snap.Handler' actions to allow you to conditionally
-- determine the setting of each header.
data CORSOptions b = CORSOptions
  { corsAllowOrigin :: Snap.Handler b CORS OriginList
  -- ^ Which origins are allowed to make cross-origin requests.

  , corsAllowCredentials :: Snap.Handler b CORS Bool
  -- ^ Whether or not to allow exposing the response when the omit credentials
  -- flag is unset.  
  }

-- | Liberal default options. Specifies that all origins may make cross-origin
-- requests, allow-credentials is true. Headers are determined unconditionally.
defaultOptions :: CORSOptions b
defaultOptions = CORSOptions
  { corsAllowOrigin = return Everywhere
  , corsAllowCredentials = return True
  }

-- | Initialize CORS for every request, unconditionally.
--
-- 'initCors' ≡ 'initCORSWithOptions' 'defaultOptions'
initCORS :: Snap.SnapletInit b CORS
initCORS = initCORSWithOptions defaultOptions

-- | Initialize CORS for all requests with specific options.
initCORSWithOptions :: CORSOptions b -> Snap.SnapletInit b CORS
initCORSWithOptions options = Snap.makeSnaplet "CORS" "Cross-origin Resource Sharing" Nothing $
  CORS <$ Snap.wrapSite (applyCORS options >>)

-- | Apply CORS headers to a specific request. This is useful if you only have
-- a single action that needs CORS headers, and you don't want to pay for
-- conditional checks on every request.
applyCORS :: CORSOptions b -> Snap.Handler b CORS ()
applyCORS options = do
  originList <- corsAllowOrigin options
  addHeader "Access-Control-Allow-Origin" (encodeOriginList originList)
  allowCredentials <- corsAllowCredentials options
  when (allowCredentials) $
    addHeader "Access-Control-Allow-Credentials" "true"

 where
   addHeader k v = Snap.modifyResponse (Snap.addHeader k v)

encodeOriginList :: OriginList -> BS.ByteString
encodeOriginList Everywhere = Char8.pack "*"
encodeOriginList Nowhere = Char8.pack "null"
encodeOriginList (Origins []) = Char8.pack "null"
encodeOriginList (Origins origins) =
  Char8.intercalate (Char8.singleton ' ') (map (Char8.pack . show) origins)
