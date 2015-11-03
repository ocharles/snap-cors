snap-cors
=========

Add CORS (cross-origin resource sharing) headers to Snap
applications. This enables web applications running on other domains
to make requests against another application.

Snaplet wrapper
===============

Making a snaplet wrapper for CORS is easy, simply use 'wrapSite':

```haskell
import qualified Snap

...
   Snap.wrapSite $ applyCORS defaultOptions
...
```