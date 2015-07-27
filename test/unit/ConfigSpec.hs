{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec where

import Test.Hspec

import Network.Tor.Config as Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Config parser" $ do
    it "parses a sample config file correctly" $ do
      fileContents <- readFile "test-data/torrc"
      let parsedConfig = Config.fromFile fileContents
      parsedConfig `shouldBe`
        Config {
          isPublicServer = True,
          orPort = 3000,
          dirPort = 3400,
          dataDirectory = "/tmp",
          -- FIXME: finish this up
          descriptorInfo = undefined, -- DescriptorInfo {},
          bandwidthInfo = undefined -- BandwidthInfo {},
                    -- FIXME: add exit policy and family
        } 
      return ()