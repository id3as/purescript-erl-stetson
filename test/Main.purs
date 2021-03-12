module Test.Main where

import Prelude
import Effect (Effect)
import Test.Handlers (testStetsonConfig, testStetsonConfig2)

type State
  = Unit

main :: Effect Unit
main = do
  -- void $ test_defaultHandler
  -- void $ test_simpleHandler
  void $ testStetsonConfig
  void $ testStetsonConfig2
  pure $ unit

-- test_defaultHandler :: Effect Unit
-- test_defaultHandler =
--   let
--     { allowMissingPost
--     , allowedMethods
--     , contentTypesAccepted
--     , contentTypesProvided
--     , deleteResource
--     , forbidden
--     , init
--     , isAuthorized
--     , isConflict
--     , movedPermanently
--     , movedTemporarily
--     , previouslyExisted
--     , serviceAvailable
--     , terminate
--     } = myHandler
--   in
--     do
--       assert $ isNothing allowMissingPost
--       assert $ isNothing allowedMethods
--       assert $ isNothing contentTypesAccepted
--       assert $ isNothing contentTypesProvided
--       assert $ isNothing deleteResource
--       assert $ isNothing forbidden
--       assert $ isNothing isAuthorized
--       assert $ isNothing isConflict
--       assert $ isNothing movedPermanently
--       assert $ isNothing movedTemporarily
--       assert $ isNothing previouslyExisted
--       assert $ isNothing serviceAvailable
--       assert $ isNothing terminate
-- test_simpleHandler :: Effect Unit
-- test_simpleHandler =
--   let
--     { allowMissingPost
--     , allowedMethods
--     , contentTypesAccepted
--     , contentTypesProvided
--     , deleteResource
--     , forbidden
--     , init
--     , isAuthorized
--     , isConflict
--     , movedPermanently
--     , movedTemporarily
--     , previouslyExisted
--     , serviceAvailable
--     , terminate
--     } = myHandler2
--   in
--     do
--       assert $ isNothing allowMissingPost
--       assert $ isJust allowedMethods
--       assert $ isNothing contentTypesAccepted
--       assert $ isNothing contentTypesProvided
--       assert $ isNothing deleteResource
--       assert $ isNothing forbidden
--       assert $ isNothing isAuthorized
--       assert $ isNothing isConflict
--       assert $ isNothing movedPermanently
--       assert $ isNothing movedTemporarily
--       assert $ isNothing previouslyExisted
--       assert $ isNothing serviceAvailable
--       assert $ isNothing terminate
