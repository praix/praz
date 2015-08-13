module Network.Tor.Config (
    fromFile
  , Config (..)
  , BandwidthInfo (..)
  , DescriptorInfo (..)
  , ConfigError (..)
  ) where

import Prelude as P
import Data.Word
import Data.Map as Map
import Data.Maybe
import Data.Char
import Data.Map as Map
import Network.Tor.Utils
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.Combinator
import Text.Read
import Control.Applicative ((<$>), (<*>), (<$), (*>))
import Control.Monad

data Config = Config {
  isPublicServer :: Bool,
  orPort :: Word16,
  dirPort :: Word16,
  dataDirectory :: String,
  descriptorInfo :: DescriptorInfo,
  bandwidthInfo :: BandwidthInfo,
  family :: [String]
  -- FIXME: add exit policy
} deriving (Show, Eq)

data DescriptorInfo = DescriptorInfo {
  contact :: String,
  nickname :: String,
  platform :: String,
  address :: String
} deriving (Show, Eq)

data BandwidthInfo = BandwidthInfo {
  bandwidthAvg :: Int,
  bandwidthBurst :: Int,
  bandwidthObserved :: Int
} deriving (Show, Eq)

data ConfigError = ParseErr ParseError | MissingConfigValue String
  deriving (Show, Eq)

fromFile :: String -> Either ConfigError Config
fromFile content = do
  kvs <- mapLeft ParseErr $ parseKeyValues content
  let updatedKvs = P.map (\(k, v) ->  (P.map toLower k, v)) kvs 
  parseConfig (Map.fromList updatedKvs)

parseConfig configMap
  = let f = field configMap in
      Config <$>
           f "ispublicserver" bool (Just True)
       <*> f "orport" num Nothing
       <*> f "dirport" num Nothing
       <*> f "datadirectory" str Nothing
       <*> parseDescriptorInfo configMap
       <*> parseBandwidthInfo configMap
       <*> f "myfamily" commaSeparated Nothing

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
           f "bandwidthavg" bandwidth (Just 1073741824)
       <*> f "bandwidthburst" bandwidth (Just 1073741824)
       <*> f "bandwidthobserved" bandwidth (Just $ 2 ^ 16)

bool = True <$ string "true" <|> False <$ string "false"
num = read <$> many1 digit
str = many1 anyChar
commaSeparated = (many1 (satisfy (/= ','))) `sepBy1` (string ", ")

bandwidth = do
  val <- read <$> many1 digit
  space
  munit <- readMaybe <$> many1 letter
  when (isNothing munit) $ fail "failed parsing bandwidth"
  let unit = fromJust munit
  return $ val * (fromJust $ P.lookup unit unitMultiplier)

data Unit = Bytes | KBytes | MBytes | GBytes | KBits | MBits | GBits deriving (Show, Read, Eq)

unitMultiplier = [(Bytes, 1), (KBytes, 10 ^ 3), (MBytes, 10 ^ 6), (GBytes, 10 ^ 9),
                  (KBits, 125), (MBits, 125 * 10 ^ 3), (GBits, 125 * 10 ^ 6)]

field :: Map String String -> String -> GenParser Char () a -> Maybe a -> Either ConfigError a
field mp fieldName parser def
  = case Map.lookup fieldName mp of
      Just value -> mapLeft ParseErr $ parse parser fieldName value
      Nothing -> maybeToEither def (MissingConfigValue fieldName) 

parseKeyValues :: String -> Either ParseError [(String, String)]
parseKeyValues file = parse kVs "(unknown)" file

kVs :: GenParser Char st [(String, String)]
kVs = do
  result <- fmap catMaybes $ many (line <|> commentLine <|> blank) 
  eof
  return result

line :: GenParser Char st (Maybe (String, String))
line = do
  key <- many1 letter
  _ <- space
  value <- manyTill anyChar (try eol)
  return $ Just (key, value) 

commentLine = Nothing <$ (string "#" *> (manyTill anyChar (try eol)))

blank = Nothing <$ (manyTill space (try eol))

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
