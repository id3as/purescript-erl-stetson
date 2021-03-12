{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-stetson"
, dependencies =
    [ "erl-binary"
    , "erl-cowboy"
    , "erl-lists"
    , "erl-maps"
    , "erl-tuples"
    , "routing-duplex"
    , "assert"
    , "console"
    , "debug"
    , "simple-json"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
