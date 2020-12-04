{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-stetson"
, dependencies = [ "erl-cowboy", "routing-duplex", "console"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
,backend = "purerl"
}
