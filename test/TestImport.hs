module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Test            as X

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    makeFoundation settings
