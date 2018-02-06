# 1.3.0

* Now just re-exports `Snap.Util.CORS`.

# 1.2.11

* Dropped dependency on `transformers`.

# 1.2.10

* Increased upper-bound on `snap`.

# 1.2.9

* Increased upper-bound on `attoparsec`.

# 1.2.8

* Now compiles with Snap 0.14.

# 1.2.7

* Now compiles with GHC 7.10. Thanks to Philip Joseph (@lambdahands) for fixing
  this.

# 1.2.6

* Increased upper bounds on `network`, `text` and `transformers`.

# 1.2.5.1

* Address deprecation warning from `attoparsec`.

# 1.2.5

* Increased upper bound `attoparsec`.

# 1.2.4

* Increased upper bound on `network`.

# 1.2.3

* Added a bit more documentation to `applyCORS`. Thanks to Alfredo Di Napoli for
  discovering this tricky behaviour.

# 1.2.2

* Increased upper bound on case-insensitive. Thanks to Ricky Elrod (@CodeBlock).

# 1.2.1

* Increased upper bounds on dependencies

# 1.2

* Added support for pre-flight requests
* Previous versions has a bug where the actual request wasn't issue after validating CORS

# 1.1

* It is now possible to specify `Access-Control-Expose-Headers`

# 1.0.1

* Allow `hashable` 1.2

# 1.0.0

* Initial release. Support setting the `Access-Control-Allow-Origin` and `Access-Control-Allow-Credentials` headers.
