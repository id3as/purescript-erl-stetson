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
        , version = "f1e3c71405f6b6d2f38086eb7ece8e197f5f8bd2"
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
        , version = "7a834e8122193bbea4d5c399786c0308b0d99537"
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
        , version = "9d8a9d0fb0e50d72fbae2f733c5a638ece2055ea"
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
        , version = "8969ab236178c3c861cf90fd55c537caa2584a9e"
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
        , version = "e6f009904aa8d14a5417d33ba8533d7d45416cb1"
        }
