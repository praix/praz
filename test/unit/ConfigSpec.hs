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
        Right Config {
          isPublicServer = True,
          orPort = 3000,
          dirPort = 3400,
          dataDirectory = "/tmp",
          -- FIXME: finish this up
          descriptorInfo = DescriptorInfo {
            contact = "danoctavian91@gmail.com",
            nickname = "shukarRelay",
            address = "localcombat",
            platform = "Tor on haskell"
          },
          bandwidthInfo = BandwidthInfo {
            bandwidthAvg = 1073741824,
            bandwidthBurst = 1000,
            bandwidthObserved = (2 ^ 16 :: Int)
          },
          family = ["troll", "orc"]       -- FIXME: add exit policy and family
        } 
      return ()
    return ()