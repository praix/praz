module Network.Tor.Config where

import Data.Word
import Data.Map as Map
import Network.Tor.Utils
import Text.ParserCombinators.Parsec
import Text.Parsec.Combinator
import Control.Applicative ((<$>), (<*>), (<$))

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

data ConfigError = ParseErr ParseError | MissingConfigValue String
  deriving (Show, Eq)

fromFile :: String -> Either ConfigError Config
fromFile content = do
  kvs <- mapLeft ParseErr $ parseKeyValues content
  return undefined

parseConfig configMap
  = let f = field configMap in
      Config <$>
           f "ispublicserver" bool (Just True)
       <*> f "orport" (read <$> many1 digit) Nothing
       <*> f "dirport" (read <$> many1 digit) Nothing
       <*> f "datadirectory" str Nothing
       <*> parseDescriptorInfo configMap
       <*> parseBandwidthInfo configMap

parseDescriptorInfo configMap
  = let f = field configMap in
      DescriptorInfo <$>
           f "contact" str Nothing
       <*> f "nickname" str Nothing
       <*> f "platform" str (Just "Tor on haskell")
       <*> f "address" str Nothing

parseBandwidthInfo configMap
  = let f = field configMap in
      BandwidthInfo <$>
           f "bandwidthavg" num (Just 1073741824)
       <*> f "bandwidthburst" num (Just 1073741824)
       <*> f "bandwidthobserved" num (Just $ 2 ^ 16)

bool = True <$ string "true" <|> False <$ string "false"
num = read <$> many1 digit
str = many1 anyChar

field :: Map String String -> String -> GenParser Char () a -> Maybe a -> Either ConfigError a
field mp fieldName parser def
  = case Map.lookup fieldName mp of
      Just value -> mapLeft ParseErr $ parse parser "(unknown)" value
      Nothing -> maybeToEither def (MissingConfigValue fieldName) 

parseKeyValues :: String -> Either ParseError [(String, String)]
parseKeyValues file = parse kVs "(unknown)" file

kVs :: GenParser Char st [(String, String)]
kVs = do
  result <- many line
  eof
  return result

line :: GenParser Char st (String, String)
line = do
  key <- many1 letter
  space
  value <- many1 anyChar
  eol
  return (key, value)  

eol :: GenParser Char st Char
eol = char '\n'

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
