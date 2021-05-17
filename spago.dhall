{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-stetson"
, dependencies =
  [ "effect"
  , "either"
  , "erl-atom"
  , "erl-cowboy"
  , "erl-lists"
  , "erl-maps"
  , "erl-modules"
  , "erl-process"
  , "erl-tuples"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "partial"
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
