    let GitHub-project =
          https://raw.githubusercontent.com/ocharles/dhall-to-cabal/bc8337b1c2c7363b5cfedbfee2aefd5b1e7e4181/dhall/GitHub-project.dhall 

in  let licenses =
          constructors
          https://raw.githubusercontent.com/ocharles/dhall-to-cabal/bc8337b1c2c7363b5cfedbfee2aefd5b1e7e4181/dhall/types/License.dhall 

in  let languages =
          constructors
          https://raw.githubusercontent.com/ocharles/dhall-to-cabal/bc8337b1c2c7363b5cfedbfee2aefd5b1e7e4181/dhall/types/Language.dhall 

in  let library =
              let unconditional =
                    https://raw.githubusercontent.com/ocharles/dhall-to-cabal/bc8337b1c2c7363b5cfedbfee2aefd5b1e7e4181/dhall/unconditional.dhall 
          
          in  unconditional.library

in  let empty-Library =
          https://raw.githubusercontent.com/ocharles/dhall-to-cabal/bc8337b1c2c7363b5cfedbfee2aefd5b1e7e4181/dhall/defaults/Library.dhall 

in    GitHub-project
      { owner = "ocharles", repo = "snap-cors" }
    ⫽ { version =
          v "1.3.0"
      , synopsis =
          "Add CORS headers to Snap applications"
      , description =
          ''
          Add CORS (cross-origin resource sharing) headers to Snap applications. This
          enables web applications running on other domains to make requests against
          another application.
          
          Since version 1.3 this library simply re-exports @Snap.Util.CORS@.''
      , license =
          licenses.BSD3 {=}
      , license-files =
          [ "LICENSE" ]
      , author =
          "Oliver Charles"
      , maintainer =
          "ollie@ocharles.org.uk"
      , copyright =
          "Oliver Charles (c) 2013"
      , category =
          "Web"
      , extra-source-files =
          [ "Changelog.md" ]
      , library =
          library
          (   empty-Library
            ⫽ { build-depends =
                  [ { package =
                        "snap-core"
                    , bounds =
                        majorBoundVersion (v "1.0.2")
                    }
                  ]
              , reexported-modules =
                  [ { name =
                        "Snap.CORS"
                    , original =
                        { name =
                            "Snap.Util.CORS"
                        , package =
                            [ "snap-core" ] : Optional Text
                        }
                    }
                  ]
              , default-language =
                  languages.Haskell2010 {=}
              }
          )
      }
