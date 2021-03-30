let base = ./spago.dhall

in    base
    ⫽ { sources =
          base.sources # [ "**/*.purs" ]
      , dependencies =
          base.dependencies # [
          , "erl-test-eunit"
          , "simple-json"
          , "erl-pinto"
          ]
      }