module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        get HomeR
        statusIs 200
        htmlAllContain "h1" "Welcome to Yesod"

        request $ do
            setMethod "POST"
            setUrl HomeR
            addToken
            fileByLabel "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
            byLabel "What's on the file?" "Some Content"

        statusIs 200
        -- more debugging printBody
        htmlCount ".message" 1
        htmlAllContain ".message" "Some Content"
        htmlAllContain ".message" "text/plain"
