{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-stetson"
, dependencies =
  [ "control"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-cowboy"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-process"
  , "erl-ssl"
  , "erl-tuples"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "prelude"
  , "record"
  , "routing-duplex"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
