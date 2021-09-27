{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
  }
-------------------------------
-}
let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

let overrides =
      { erl-lists =
              upstream.erl-lists
          //  { version = "1d059f0df04f1c83f35a6eae706bd86cda8b015e" }
       , erl-binary =
        { dependencies = [ "prelude", "erl-lists" ]
        , repo = "https://github.com/id3as/purescript-erl-binary.git"
        , version = "423f1af8437670beab03463b3e9bc0a487f05ba4"
        }
      , erl-cowboy =
        { repo = "https://github.com/purerl/purescript-erl-cowboy.git"
        , dependencies =
          [ "console"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-kernel"
          , "erl-lists"
          , "erl-maps"
          , "erl-modules"
          , "erl-ranch"
          , "erl-tuples"
          , "foreign"
          , "functions"
          , "maybe"
          , "prelude"
          , "transformers"
          , "tuples"
          , "unsafe-coerce"
          ]
        , version = "ca4dd4a6432817fbe6ef9ab1814046f6827046cd"
        }
      , erl-ranch =
        { dependencies =
          [ "convertable-options"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-kernel"
          , "erl-lists"
          , "erl-maps"
          , "erl-otp-types"
          , "erl-process"
          , "erl-ssl"
          , "erl-tuples"
          , "exceptions"
          , "foreign"
          , "maybe"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-ranch.git"
        , version = "08a76bd850ba00c3a120c1d149bed07f9fcc165d"
        }
      , erl-process =
        { repo = "https://github.com/id3as/purescript-erl-process.git"
        , dependencies =
          [ "console"
          , "prelude"
          , "effect"
          ]
        , version = "afbfa4e7a13c0d55609ff144d49982563fada7f5"
        }
      , erl-otp-types =
        { dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-kernel"
          , "foreign"
          , "prelude"
          , "unsafe-reference"
          ]
        , repo = "https://github.com/id3as/purescript-erl-otp-types.git"
        , version = "6470bc379447c406456e8ef1e6a79c80e3c5e8d1"
        }      
    , erl-ssl =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "maybe"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-kernel"
          , "erl-tuples"
          , "erl-logger"
          , "erl-otp-types"
          , "foreign"
          , "maybe"
          , "partial"
          , "prelude"
          , "record"
          , "unsafe-reference"
          ]
        , repo = "https://github.com/id3as/purescript-erl-ssl.git"
        , version = "2bd94ce343448406e579425e1b4140a6b6dd7de0"
        }
        , unsafe-reference =
        { repo = "https://github.com/purerl/purescript-unsafe-reference.git"
        , dependencies = [ "prelude"  ]
        , version = "464ee74d0c3ef50e7b661c13399697431f4b6251"
        }
      }

let additions = {
 erl-pinto =
        { repo = "https://github.com/id3as/purescript-erl-pinto.git"
        , dependencies =
          [ "erl-process"
          , "erl-lists"
          , "erl-atom"
          , "erl-tuples"
          , "erl-modules"
          , "foreign"
          ]
        , version = "f4200a0ed787c74649e4f2503a77df3c6cb5c21d"
        }
      , erl-kernel =
        { dependencies =
          [ "convertable-options"
          , "datetime"
          , "effect"
          , "either"
          , "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-process"
          , "erl-tuples"
          , "erl-untagged-union"
          , "foldable-traversable"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "newtype"
          , "partial"
          , "prelude"
          , "record"
          , "typelevel-prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-kernel.git"
        , version = "2c1f78a3aa6993e91e342a984c522b87b98bbb2b"
        }
      , convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
      , erl-untagged-union =
        { dependencies =
          [ "erl-atom"
          , "erl-binary"
          , "erl-lists"
          , "erl-tuples"
          , "debug"
          , "foreign"
          , "typelevel-prelude"
          , "maybe"
          , "partial"
          , "prelude"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
        , version = "eb7a10c7930c4b99f1a6bfce767daa814d45dd2b"
        }

}

in  upstream // overrides // additions
