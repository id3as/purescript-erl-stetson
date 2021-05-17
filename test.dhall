let base = ./spago.dhall

in    base
    ⫽ { sources =
          base.sources # [ "test/**/*.purs" ]
      , dependencies =
          base.dependencies # [
          , "simple-json"
          , "erl-pinto"
          , "assert"
          , "erl-binary"
          , "newtype"
          , "erl-test-eunit"
          , "free"
          , "debug"
          ]
      }