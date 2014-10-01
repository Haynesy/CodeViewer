module FooSpec (main, spec) where

    import Test.Hspec

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = describe "Foo" $ do
            it "A Foo" $ "Foo" `shouldBe` "Foo"
            it "A Bar" $ "Bar" `shouldBe` "Bar"
