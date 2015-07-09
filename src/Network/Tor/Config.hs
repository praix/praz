module Network.Tor.Config where

import Data.Word

import Data.Map as Map

import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator

data Config = Config {
  isPublicServer :: Bool,
  orPort :: Word16,
  dirPort :: Word16,
  dataDirectory :: String,
  descriptorInfo :: DescriptorInfo,
  bandwidthInfo :: BandwidthInfo,
  family :: [String]
  -- FIXME: add exit policy
}

data DescriptorInfo = DescriptorInfo {
  contact :: String,
  nickname :: String,
  platform :: String,
  address :: String
}

data BandwidthInfo = BandwidthInfo {
  bandwidthAvg :: Int,
  bandwidthBurst :: Int,
  bandwidthObserved :: Int
}


{-
fromFile :: String -> Maybe Config
fromFile content = undefined


parseKeyValues :: String -> Maybe (Map String String)
parseKeyValues = undefined

parseKVs :: GenParser Char st [[String]]
parseKVs = do
  result <- many line
  eof
  return result

line :: GenParser Char st [String]
line = do
  key <- many1 letter

  eol                       -- end of line
  return result  

eol :: GenParser Char st Char
eol = char '\n'

-}
{-
 as defined in golang
data ExitRule struct {
  Address []byte
  Port    uint16
  Action  bool
  V6      bool
}

type ExitPolicy struct {
  // The "zero value" of this struct is a "reject *:*"
  Rules         []ExitRule
  DefaultAction bool
}
-}
