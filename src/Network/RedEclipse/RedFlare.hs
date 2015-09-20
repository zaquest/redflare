{-| This library provides functions and datatypes for getting information
  about Red Eclipse's game servers.
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.RedEclipse.RedFlare
                (serversList
                ,serverQuery
                ,redFlare
                ,ServerReport(..)
                ,Version(..)
                ,VerInfo(..)
                ,Mode(..)
                ,Mutator(..)
                ,MasterMode(..)
                ,GameState(..)
                ,Platform(..)
                ,HostName
                ,PortNumber) where

import Control.Exception (bracket)
import Control.Monad (replicateM, filterM, unless, when, zipWithM_)
import Control.Monad.State (State, runState, evalState, put, get, modify)
import Data.Binary.Get
import Data.Bits (Bits, testBit, clearBit, zeroBits)
import Data.Either (rights, either)
import Data.List (isPrefixOf, dropWhileEnd, foldl1')
import Data.Maybe (mapMaybe, maybe, fromJust, isNothing)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import Data.Word (Word8)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LW
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..))
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import System.Timeout (timeout)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

-- * Red Flare

-- | Takes Red Eclipse server's hostname and port number and returns
-- either error string or server's state report.
serverQuery :: HostName -> PortNumber -> IO (Either String ServerReport)
serverQuery host port = withSocket host (port+1) Datagram (((>>= parseReport) <$>) . recvReport)
  where
    recvReport Nothing = return (Left ("Can't connect to " ++ host ++ ":" ++ show port))
    recvReport (Just sock) = do
      sendAll sock (W.pack ping) -- ping id
      maybe (Left "Timed out on receiving report") Right <$> timeout 10000000 (recv sock enetMaxMTU)
    parseReport reportBS =
      case runGetOrFail (skip (length ping) >> getREServerReport) (LW.fromStrict reportBS) of
        Left (_, _, err) -> Left err
        Right (_, _, srv) -> Right srv
    ping = [0x02, 0x00]

-- | Takes master server's hostname and port number. Returns a list
-- of server's connected to that master server.
serversList :: HostName -> PortNumber -> IO [(HostName, PortNumber)]
serversList host port = parseServersCfg <$> withSocket host port Stream getServersCfg
  where
    getServersCfg Nothing = fail ("Can't connect " ++ host ++ ":" ++ show port)
    getServersCfg (Just sock) = do
      sendAll sock (C.pack "update\n")
      recvAll sock
    recvAll :: Socket -> IO W.ByteString
    recvAll sock = do
      mchunk <- timeout 10000000 (recv sock enetMaxMTU)
      case mchunk of
        Nothing -> fail "Timed out on receiving servers list"
        Just chunk -> if W.length chunk > 0
                        then (chunk <>) <$> recvAll sock
                        else return W.empty
    parseServersCfg = mapMaybe parseAddServer . filter isAddServer . lines . C.unpack
    parseAddServer line =
      case drop 1 (words line) of
        (host:portString:_) ->
          case reads portString :: [(Int, String)] of
            [(port, _)] -> Just (host, fromIntegral port)
            _ -> Nothing
        _ -> Nothing
    isAddServer = isPrefixOf "addserver"

-- | Takes master server's hostname and port number, requests a list of
-- connected servers and polls them to get their current state.
-- Returns a list of tuples where first element contains
-- server's address information and second contains either error string or
-- server's state report.
--
-- This function runs `serversList`, than maps `serverQuery` over the
-- result of `serversList` and zips results of both functions.
redFlare :: HostName -> PortNumber -> IO [((HostName, PortNumber), Either String ServerReport)]
redFlare host port = do
  servers <- serversList host port
  zip servers <$> mapConcurrently (uncurry serverQuery) servers

-- | Red Eclipse uses enet library. Enet transfers data in UDP packets
-- of length no more than 4096.
enetMaxMTU :: Int
enetMaxMTU = 4096

-- | Parse Red Eclipse's integer compression.
getREInt :: Get Int
getREInt =  do
  w1 <- getWord8
  case w1 of
    128 -> fromIntegral <$> getWord16le
    129 -> fromIntegral <$> getWord32le
    _ -> return (fromIntegral w1)

-- String functions
data UncolorState = NoColor | ColorSeq | ColorParen | ColorBracket | ColorBlink Int
  deriving Eq

-- | Strip Red Eclipse's color codes from string
uncolorString :: String -> String
uncolorString s = evalState (filterM dropColor s) NoColor
  where
    dropColor :: Char -> State UncolorState Bool
    dropColor '\f' = put ColorSeq >> return False
    dropColor '[' = dropOn ColorSeq ColorBracket
    dropColor ']' = dropOn ColorBracket NoColor
    dropColor '(' = dropOn ColorSeq ColorParen
    dropColor ')' = dropOn ColorParen NoColor
    dropColor 'z' = dropOn ColorSeq (ColorBlink 2)
    dropColor x = do s <- get
                     case s of
                       NoColor -> return True
                       ColorSeq -> put NoColor >> return False
                       ColorBlink 0 -> put NoColor >> return True
                       ColorBlink x -> put (ColorBlink (x-1)) >> return False
                       _ -> return False
    dropOn s ns = do st <- get
                     if st == s
                       then put ns >> return False
                       else return True

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
getREString :: Get String
getREString = (map wordToChar . LW.unpack) <$> getLazyByteStringNul

-- | Parse enum type `a` from Red Eclipse's integer. `name` is enum's name
-- used for error reporting.
getREEnum :: forall a. (Enum a, Bounded a) => String -> Get a
getREEnum name = do
  num <- getREInt
  if num `elem` map fromEnum (enumFrom minBound :: [a])
    then return (toEnum num)
    else fail ("Unknown " ++ name ++ " " ++ show num)

-- | Datatype to represent Red Eclipse's protocol version
data Version = V220 | V226 | V227
  deriving (Show, Read, Eq, Ord)

tableVersion :: [(Version, Int)]
tableVersion = [(V220, 220), (V226, 226), (V227, 227)]

instance Enum Version where
  fromEnum = fromJust . flip lookup tableVersion
  toEnum = fromJust . flip lookup (map swap tableVersion)
  succ = head . tail . enumFrom
  pred v = last . init $ dropWhileEnd (/= v) (map fst tableVersion)
  enumFrom v = dropWhile (/= v) (map fst tableVersion)
  enumFromTo v v' = dropWhileEnd (/= v') (enumFrom v)

instance Bounded Version where
  minBound = V220
  maxBound = V227

-- | Parse protocol's version number.
getREVersion :: Get Version
getREVersion = getREEnum "Red Eclipse version"

-- | Datatype to represent Red Eclipse server's version information.
data VerInfo = VerInfo {
  versionMajor    :: Int,      -- ^ Major Red Eclipse version
  versionMinor    :: Int,      -- ^ Minor Red Eclipse version
  versionPatch    :: Int,      -- ^ Patch Red Eclipse version
  versionPlatform :: Platform, -- ^ Platform for which Red Eclipse was compiled
  versionArch     :: Int       -- ^ Architecture for which Red Eclipse was compiled
} deriving (Show, Read)

-- | Datatype to represent platform
data Platform = Win | OSX | LinuxBSD
  deriving (Show, Read, Enum, Bounded)

-- | Parse Red Eclipse server's version info
getREVerInfo :: Get VerInfo
getREVerInfo = VerInfo <$> getREInt
                       <*> getREInt
                       <*> getREInt
                       <*> getREPlatform
                       <*> getREInt

getREPlatform :: Get Platform
getREPlatform = getREEnum "platform"

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

-- | Game mode.
data Mode = Demo | Edit | Deathmatch | Capture | Defend | Bomber | Race | Gauntlet
  deriving (Show, Eq, Ord, Enum, Read, Bounded)

-- | Parse game mode.
-- Currently the version is not needed. The only caveat of ignoring version
-- is that in case of an error there might appear `Gauntlet` in 1.5 protocol.
getREMode :: Version -> Get Mode
getREMode _ = getREEnum "game mode"

-- | Game mutator.
data Mutator = Multi   | FFA       | Coop        | Insta     | Medieval
             | Kaboom  | Duel      | Survivor    | Classic   | OnSlaught
             | Jetpack | Freestyle | Vampire     | Expert    | Resize
             | Hard    | Basic     | Quick       | MutDefend | Protect
             | King    | Hold      | Basket      | Touchdown | Attack
             | Timed   | Endurance | MutGauntlet
  deriving (Show, Read, Eq)

-- | Returns a list of available mutators for the given version and game mode.
-- Mutators are ordered in Red Eclipse's enum order. If n-th mutator
-- in Red Eclipse's order is not available in given mode and version
-- then `Nothing` is put in on n-th position in output list.
verModeMuts :: Version -> Mode -> [Maybe Mutator]
verModeMuts V220 Edit       = [FFA, Classic, Jetpack] `from` allMuts V220
verModeMuts v    Edit       = [FFA, Classic, Freestyle] `from` allMuts v
verModeMuts v    Deathmatch = allow (allMuts v)
verModeMuts v    Capture    = allMuts v `but` [FFA] ++ allow [Quick, MutDefend, Protect]
verModeMuts v    Defend     = allMuts v `but` [FFA, Duel, Survivor] ++ allow [Quick, King]
verModeMuts V220 Bomber     = allMuts V220 `but` [FFA] ++ allow [Hold, Touchdown]
verModeMuts v    Bomber     = allMuts v `but` [FFA] ++ allow [Hold, Basket, Attack]
verModeMuts V220 Race       = allMuts V220 `but` [Multi, Coop, Duel, Survivor]
verModeMuts v    Race       = [Multi, FFA, OnSlaught, Freestyle] `from` allMuts v ++ allow [Timed, Endurance, MutGauntlet]
verModeMuts V220 Gauntlet   = allMuts V220 `but` [Multi, FFA, Coop, Survivor] ++ allow [Timed, Hard]
verModeMuts _    Gauntlet   = []

allMuts V220 = [Multi, FFA, Coop, Insta, Medieval, Kaboom, Duel, Survivor, Classic, OnSlaught, Jetpack, Vampire, Expert, Resize]
allMuts _    = [Multi, FFA, Coop, Insta, Medieval, Kaboom, Duel, Survivor, Classic, OnSlaught, Freestyle, Vampire, Resize, Hard, Basic]
ys `from` xs = map (\x -> if x `elem` ys then Just x else Nothing) xs
xs `but`  ys = map (\x -> if x `elem` ys then Nothing else Just x) xs
allow = map Just

getREMutators :: Version -> Mode -> Get [Mutator]
getREMutators version mode = do
  num <- getREInt
  let emuts = bitsToList (verModeMuts version mode) num
  either (fail' num) return emuts
  where
    fail' num _ = fail ("Malformed mutator code " ++ show num ++ " for " ++ show mode ++ " in protocol " ++ show version)

-- | Master mode.
data MasterMode = Open | Veto | Locked | Private | Password
  deriving (Show, Read, Enum, Bounded)

-- | Parse master mode.
getREMasterMode :: Get MasterMode
getREMasterMode = getREEnum "master mode"

-- | Datatype to represent game state.
data GameState = Waiting | GetMap | SendMap | Readying | GameInfo | Playing | Overtime | Intermission | Voting
  deriving (Show, Read, Enum, Bounded)

-- | Parse game state.
getREGameState :: Version -> Get GameState
getREGameState V220 = fail "V220 does not support game state"
getREGameState v = do
  idx <- getREInt
  when (idx < 0) (fail' idx)
  case drop idx states of
    (gs:_) -> return gs
    []     -> fail' idx
  where
    states = case v of
               V226 -> [Waiting, Voting, Intermission, Playing, Overtime]
               V227 -> enumFrom Waiting
    fail' idx = fail ("Bad game state " ++ show idx ++ " in protocol version " ++ show v)

-- | Parse server's report.
getREServerReport :: Get ServerReport
getREServerReport = do
  playerCnt <- getREInt
  _attrCnt <- getREInt
  version <- getREVersion
  gameMode <- getREMode version
  mutators <- getREMutators version gameMode
  timeRemaining <- getREInt
  serverClients <- getREInt
  masterMode <- getREMasterMode
  numGameVars <- getREInt
  numGameMods <- getREInt
  (verInfo, gameState, timeLeft) <- case version of
      V220 -> return (Nothing, Nothing, Nothing)
      _ -> (,,) <$> (Just <$> getREVerInfo)
                <*> (Just <$> getREGameState version)
                <*> (Just <$> getREInt)
  mapName <- getREString
  serverDesc <- uncolorString <$> getREString
  verBranch <- case version of
    V227 -> Just <$> getREString
    _ -> return Nothing
  playerNames <- map uncolorString <$> replicateM playerCnt getREString
  handles <- case version of
    V220 -> return Nothing
    _ -> Just <$> replicateM playerCnt getREString
  return ServerReport {..}

-- * General utils

-- | A function to connect to server on specified host and port using
-- specified socket type. If connection failed function argument is
-- `Nothing` otherwise it's `(Just socket)`.
withSocket :: HostName -> PortNumber -> SocketType -> (Maybe Socket -> IO a) -> IO a
withSocket host port sockType = bracket conn (maybe (return ()) sClose)
  where
    conn = do
      addrInfos <- getAddrInfo (Just defaultHints { addrFamily = AF_INET
                                                  , addrSocketType = sockType })
                               (Just host)
                               (Just (show port))
      case addrInfos of
        (serverAddr:_) -> do
          sock <- socket (addrFamily serverAddr) sockType defaultProtocol
          connect sock (addrAddress serverAddr)
          return (Just sock)
        _ -> return Nothing

-- | Splits bitfield of type `b` into a list of values `[a]`.
-- Input list `[Maybe a]` is interpreted in the following way: if
-- n-th value in a list is `Nothing` then n-th bit in the bitfield must
-- be `0` else fail. `Just` values are unwrapped. If n-th bit is set
-- in the bitfield and the length of the input list is `<= n` then fail.
bitsToList :: Bits b => [Maybe a] -> b -> Either String [a]
bitsToList xs b = if leftover == zeroBits
                    then Right (map (\(_, Just m, _) -> m) muts)
                    else Left "Malformed enum code"
  where
    (muts, leftover) = runState (filterM removeCode (zip3 (bits b) xs [0..])) b
    removeCode (b, ma, n) = unless (isNothing ma) (modify (`clearBit` n)) >> return b

-- | Returns a bits of `b` as a list of `Bool` values.
bits :: Bits b => b -> [Bool]
bits b = map (testBit b) [0..]

mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
mapConcurrently f as = do
  vars <- mapM (const newEmptyMVar) as
  zipWithM_ inOwnThread vars as
  collect vars
  where
    inOwnThread v a = forkIO $ f a >>= putMVar v
    collect = mapM takeMVar

-- | ToJSON instances
deriveToJSON defaultOptions { omitNothingFields = True } ''ServerReport
deriveToJSON defaultOptions { omitNothingFields = True } ''VerInfo
deriveToJSON defaultOptions { omitNothingFields = True } ''Mode
deriveToJSON defaultOptions { omitNothingFields = True } ''MasterMode
deriveToJSON defaultOptions { omitNothingFields = True } ''GameState
deriveToJSON defaultOptions { omitNothingFields = True } ''Platform

instance ToJSON Mutator where
  toJSON MutDefend = toJSON "Defend"
  toJSON MutGauntlet = toJSON "Gauntlet"
  toJSON mut = toJSON (show mut)

instance ToJSON PortNumber where
  toJSON p = toJSON (fromIntegral p :: Int)

instance ToJSON Version where
  toJSON = toJSON . fromEnum
