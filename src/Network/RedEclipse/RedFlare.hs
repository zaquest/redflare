{-| This library provides functions and datatypes for getting information
  about Red Eclipse's game servers.
 -}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Network.RedEclipse.RedFlare
                (serversList
                ,serverQuery
                ,redFlare
                ,ServerReport(..)
                ,VerInfo(..)
                ,Version
                ,Mode
                ,Mutator
                ,MasterMode
                ,GameState
                ,Platform
                ,Address(..)
                ,host
                ,port
                ,HostName
                ,PortNumber) where

import Prelude hiding (take, takeWhile)
import Control.Exception (bracket)
import Control.Monad (replicateM, filterM, unless, when, zipWithM_, (<$!>))
import Data.Bits (Bits, testBit, zeroBits, (.|.), shift)
import Data.Either (rights, either)
import Data.Maybe (mapMaybe, maybe, fromJust)
import Data.Word (Word8)
import Network.Fancy
import qualified Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..))
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import System.Timeout (timeout)
import qualified System.IO as S
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.DeepSeq (force)
import Data.Attoparsec.Zepto

type PortNumber = Int

host :: Address -> HostName
host (IP   h _) = h
host (IPv4 h _) = h
host (IPv6 h _) = h
host (Unix h)   = h

port :: Address -> Maybe PortNumber
port (IP   _ p) = Just p
port (IPv4 _ p) = Just p
port (IPv6 _ p) = Just p
port (Unix _)   = Nothing

incPort :: Address -> Address
incPort (IP   h p) = IP   h (p+1)
incPort (IPv4 h p) = IPv4 h (p+1)
incPort (IPv6 h p) = IPv6 h (p+1)
incPort (Unix h)   = Unix h

-- * Red Flare

-- | Takes Red Eclipse server's hostname and port number and returns
-- either error string or server's state report.
serverQuery :: Address -> IO (Either String ServerReport)
serverQuery addr = withDgram addr (((>>= parseReport') <$>) . recvReport)
  where
    recvReport sock = do
      send sock (W.pack ping) -- ping id
      maybe (Left "Timed out on receiving report") Right <$> timeout 10000000 (recv sock enetMaxMTU)
    parseReport' = parse parseReport . W.drop (length ping)
    ping = [0x02, 0x00]
    -- | Red Eclipse uses enet library. Enet transfers data in UDP packets
    -- of length no more than 4096.
    enetMaxMTU = 4096

-- | Takes master server's hostname and port number. Returns a list
-- of server's connected to that master server.
serversList :: Address -> IO [Address]
serversList addr = parseServersCfg <$> withStream addr getServersCfg
  where
    getServersCfg handle = do
      C.hPut handle (C.pack "update\n")
      S.hFlush handle
      force <$!> C.hGetContents handle
    parseServersCfg = mapMaybe parseAddServer . lines . C.unpack
    parseAddServer line =
      case words line of
        ("addserver":host:portString:_) -> Just $ IP host (read portString)
        _ -> Nothing

-- | Takes master server's hostname and port number, requests a list of
-- connected servers and polls them to get their current state.
-- Returns a list of tuples where first element contains
-- server's address information and second contains either error string or
-- server's state report.
--
-- This function runs `serversList`, than maps `serverQuery` over the
-- result of `serversList` and zips results of both functions.
redFlare :: Address -> IO [(Address, Either String ServerReport)]
redFlare addr = do
  servers <- serversList addr
  zip servers <$> mapConcurrently (serverQuery . incPort) servers

-- | Parse Red Eclipse's integer compression.
reInt :: Parser Int
reInt =  do
  w1 <- W.head <$> take 1
  case w1 of
    128 -> fromLE <$> take 2
    129 -> fromLE <$> take 4
    _ -> return (fromIntegral w1)
  where fromLE = foldr1 (.|.) . zipWith (flip shift) [0,8..] . map fromIntegral . W.unpack

-- String functions
uncolorString :: String -> String
uncolorString ('\f':'[':cs) = uncolorString . drop 1 $ dropWhile (/= ']') cs
uncolorString ('\f':'(':cs) = uncolorString . drop 1 $ dropWhile (/= ')') cs
uncolorString ('\f':'z':cs) = uncolorString $ drop 2 cs
uncolorString ('\f':_  :cs) = uncolorString cs
uncolorString (     c  :cs) = c : uncolorString cs
uncolorString           []  = []

-- | Convert byte to unicode character supported by Red Eclipse.
wordToChar :: Word8 -> Char
wordToChar w = cube2UniChars V.! fromIntegral w

-- | Lookup table for Red Eclipse's unicode chars.
cube2UniChars :: V.Vector Char
cube2UniChars = V.fromList $ map toEnum [
    0, 192, 193, 194, 195, 196, 197, 198, 199, 9, 10, 11, 12, 13, 200, 201,
    202, 203, 204, 205, 206, 207, 209, 210, 211, 212, 213, 214, 216, 217, 218, 219,
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
    64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
    96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
    112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 220,
    221, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
    238, 239, 241, 242, 243, 244, 245, 246, 248, 249, 250, 251, 252, 253, 255, 0x104,
    0x105, 0x106, 0x107, 0x10C, 0x10D, 0x10E, 0x10F, 0x118, 0x119, 0x11A, 0x11B, 0x11E, 0x11F, 0x130, 0x131, 0x141,
    0x142, 0x143, 0x144, 0x147, 0x148, 0x150, 0x151, 0x152, 0x153, 0x158, 0x159, 0x15A, 0x15B, 0x15E, 0x15F, 0x160,
    0x161, 0x164, 0x165, 0x16E, 0x16F, 0x170, 0x171, 0x178, 0x179, 0x17A, 0x17B, 0x17C, 0x17D, 0x17E, 0x404, 0x411,
    0x413, 0x414, 0x416, 0x417, 0x418, 0x419, 0x41B, 0x41F, 0x423, 0x424, 0x426, 0x427, 0x428, 0x429, 0x42A, 0x42B,
    0x42C, 0x42D, 0x42E, 0x42F, 0x431, 0x432, 0x433, 0x434, 0x436, 0x437, 0x438, 0x439, 0x43A, 0x43B, 0x43C, 0x43D,
    0x43F, 0x442, 0x444, 0x446, 0x447, 0x448, 0x449, 0x44A, 0x44B, 0x44C, 0x44D, 0x44E, 0x44F, 0x454, 0x490, 0x491 ]

-- | Parse Red Eclipse's string into Haskell's string.
reString :: Parser String
reString = (map wordToChar . W.unpack) <$> (takeWhile (/= 0) <* take 1)

-- | Parse value's index in `xs` list from input stream
oneOf :: String -> [a] -> Parser a
oneOf what xs = do
  idx <- reInt
  case drop idx xs of
    (x:_) -> return x
    []    -> fail (what ++ " is out of range " ++ show idx)

-- | Parse bit mask from input stream and apply it to `xs` list
listOf :: [a] -> Parser [a]
listOf xs = map fst . filter snd . zip xs . bits <$> reInt

-- | Datatype to represent Red Eclipse server's version information.
data VerInfo = VerInfo {
  versionMajor    :: Int,      -- ^ Major Red Eclipse version
  versionMinor    :: Int,      -- ^ Minor Red Eclipse version
  versionPatch    :: Int,      -- ^ Patch Red Eclipse version
  versionPlatform :: Platform, -- ^ Platform for which Red Eclipse was compiled
  versionArch     :: Int       -- ^ Architecture for which Red Eclipse was compiled
} deriving (Show, Read)

type Platform = String
platforms = ["Win", "OSX", "Linux/BSD"]

-- | Parse Red Eclipse server's version info
reVerInfo :: Parser VerInfo
reVerInfo = VerInfo <$> reInt
                    <*> reInt
                    <*> reInt
                    <*> oneOf "platform" platforms
                    <*> reInt

type Version = Int
versions = [220, 226, 229]

validateVersion :: Version -> Parser Version
validateVersion v | v `elem` versions = return v
                  | otherwise = fail ("unknown version " ++ show v)

-- | Datatype to represent current state of Red Eclipse server
data ServerReport = ServerReport {
  playerCnt     :: Int,             -- ^ Number of players currently connected to a server
  version       :: Version,         -- ^ Protocol's version
  gameMode      :: Mode,            -- ^ Game mode
  mutators      :: [Mutator],       -- ^ List of game mutators
  timeRemaining :: Int,             -- ^ Time remaining before the end of a current match
  serverClients :: Int,             -- ^ Maximum number of players that can connect to a server
  masterMode    :: MasterMode,      -- ^ Server's mode
  numGameVars   :: Int,
  numGameMods   :: Int,
  verInfo       :: Maybe VerInfo,   -- ^ Server's version information
  gameState     :: Maybe GameState, -- ^ Current state of the game
  timeLeft      :: Maybe Int,       -- ^ Time left before the end of a current match or overtime
  mapName       :: String,          -- ^ Name of a map currently being played
  serverDesc    :: String,          -- ^ Server's name
  verBranch     :: Maybe String,    -- ^ More version information
  playerNames   :: [String],        -- ^ List of players' names currently connected to a server
  handles       :: Maybe [String]   -- ^ List of players' authentication handles in the same order
                                    -- as `playerNames`. If player is not authenticated the handle is empty string.
} deriving (Show, Read)

type Mode = String
modes = ["demo", "edit", "deathmatch", "capture-the-flag", "defend-and-control", "bomber-ball", "race", "gauntlet"]

-- | Game mutator.
type Mutator = String

versionMuts :: Version -> [Mutator]
versionMuts 220 = ["multi", "ffa", "coop", "insta", "medieval", "kaboom", "duel", "survivor", "classic", "onslaught", "jetpack", "vampire", "expert", "resize"]
versionMuts _   = ["multi", "ffa", "coop", "insta", "medieval", "kaboom", "duel", "survivor", "classic", "onslaught", "freestyle", "vampire", "resize", "hard", "basic"]

gameSpecificMuts :: Version -> Mode -> [Mutator]
gameSpecificMuts 229 "deathmatch"         = ["gladiator", "oldschool"]
gameSpecificMuts _   "capture-the-flag"   = ["quick", "defend", "protect"]
gameSpecificMuts _   "defend-and-control" = ["quick", "king"]
gameSpecificMuts 220 "bomber-ball"        = ["hold", "touchdown"]
gameSpecificMuts _   "bomber-ball"        = ["hold", "basket", "attack"]
gameSpecificMuts 220 "race"               = ["timed", "endurance", "gauntlet"]
gameSpecificMuts 220 "gauntlet"           = ["timed", "hard"]
gameSpecificMuts _   _                    = []

mutatorsOf :: Version -> Mode -> [Mutator]
mutatorsOf version mode = versionMuts version ++ gameSpecificMuts version mode

type MasterMode = String
masterModes = ["open", "veto", "locked", "private", "password"]

type GameState = String
gameStates :: Version -> [GameState]
gameStates 226 = ["waiting", "voting", "intermission", "playing", "overtime"]
gameStates 229 = ["waiting", "get-map", "send-map", "readying", "game-info", "playing", "overtime", "intermission", "voting"]
gameStates _   = []

parseIf :: Bool -> Parser a -> Parser (Maybe a)
parseIf False _ = return Nothing
parseIf True  p = Just <$> p

-- | Parse server's report.
parseReport :: Parser ServerReport
parseReport = do
  playerCnt <- reInt
  _attrCnt <- reInt
  version <- reInt >>= validateVersion
  gameMode <- oneOf "mode" modes
  mutators <- listOf (mutatorsOf version gameMode)
  timeRemaining <- reInt
  serverClients <- reInt
  masterMode <- oneOf "master mode" masterModes
  numGameVars <- reInt
  numGameMods <- reInt
  verInfo <- parseIf (version /= 220) reVerInfo
  gameState <- parseIf (version /= 220) (oneOf "game state" (gameStates version))
  timeLeft <- parseIf (version /= 220) reInt
  mapName <- reString
  serverDesc <- uncolorString <$> reString
  verBranch <- parseIf (version == 229) reString
  playerNames <- map uncolorString <$> replicateM playerCnt reString
  handles <- parseIf (version /= 220) (replicateM playerCnt reString)
  return ServerReport {..}

-- * General utils

-- | Returns a bits of `b` as a list of `Bool` values.
bits :: Bits b => b -> [Bool]
bits b = map (testBit b) [0..]

mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
mapConcurrently f as = do
  vars <- mapM (const newEmptyMVar) as
  zipWithM_ inOwnThread vars as
  collect vars
  where inOwnThread v a = forkIO $ f a >>= putMVar v
        collect = mapM takeMVar

-- | ToJSON instances
deriveToJSON defaultOptions { omitNothingFields = True } ''ServerReport
deriveToJSON defaultOptions { omitNothingFields = True } ''VerInfo
