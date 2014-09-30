module FooSpec (main, spec) where

    import Test.Hspec

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = describe "Foo" $
            it "A Foo" $
                "Foo" `shouldBe` "Foo"
