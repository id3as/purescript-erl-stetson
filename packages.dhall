let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

in upstream
  with erl-lists.version = "1d059f0df04f1c83f35a6eae706bd86cda8b015e"
  with erl-binary =
        { dependencies = [ "prelude", "erl-lists" ]
        , repo = "https://github.com/id3as/purescript-erl-binary.git"
        , version = "423f1af8437670beab03463b3e9bc0a487f05ba4"
        }
  with erl-cowboy =
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
  with erl-ranch =
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
  with erl-process =
        { repo = "https://github.com/id3as/purescript-erl-process.git"
        , dependencies =
          [ "console"
          , "prelude"
          , "effect"
          ]
        , version = "afbfa4e7a13c0d55609ff144d49982563fada7f5"
        }
  with erl-otp-types =
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
  with erl-ssl =
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
  with unsafe-reference =
        { repo = "https://github.com/purerl/purescript-unsafe-reference.git"
        , dependencies = [ "prelude"  ]
        , version = "464ee74d0c3ef50e7b661c13399697431f4b6251"
        }
  with erl-pinto =
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
  with erl-kernel =
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
  with convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
  with erl-untagged-union =
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
