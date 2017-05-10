{-| This library provides functions and datatypes for getting information
  about Red Eclipse's game servers.
 -}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Network.RedEclipse.RedFlare
                (serversList
                ,serverQuery
                ,redFlare
                ,Result(..)
                ,Report(..)
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
import Control.Monad (replicateM, zipWithM_, (<$!>), join)
import Data.Bits (Bits, testBit, zeroBits, (.|.), shift)
import Data.Maybe (mapMaybe, maybe)
import Data.Word (Word8)
import Network.Fancy
import qualified Data.ByteString as W
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Data.Aeson (ToJSON(..))
import Data.Aeson.TH (deriveToJSON, defaultOptions, Options(..))
import qualified System.Timeout as Timeout
import qualified System.IO as S
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.DeepSeq (force)
import Data.Attoparsec.Zepto
import Control.Exception (catch, SomeException)

type Result a = Either String a

timeout :: String -> Int -> IO a -> IO (Result a)
timeout place duration action = do
  emresult <- (Right <$> Timeout.timeout duration action) `catch` resultCatch
  return . join $ maybe (Left $ "timeout: " ++ place) Right <$> emresult

resultCatch :: Monad m => SomeException -> m (Result a)
resultCatch = return . Left . show

secs :: Int -> Int
secs = (* 10^6)

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
serverQuery :: Address -> IO (Result Report)
serverQuery addr = do
  rreport <- timeout "on receiving report" (secs 10) (withDgram addr getReport)
  return $ (W.drop (length ping) <$> rreport) >>= parse reReport
  where
    getReport sock = do
      send sock (W.pack ping) -- ping id
      recv sock enetMaxMTU
    ping = [0x02, 0x00]
    -- | Red Eclipse uses enet library. Enet transfers data in UDP packets
    -- of length no more than 4096.
    enetMaxMTU = 4096

-- | Takes master server's hostname and port number. Returns a list
-- of server's connected to that master server.
serversList :: Address -> IO (Result [Address])
serversList masterAddr = do
  rcfg <- timeout "on receiving servers list" (secs 10) (withStream masterAddr getServersCfg)
  return (parseServers <$> rcfg)
  where
    getServersCfg handle = do
      C.hPut handle (C.pack "list\n")
      S.hFlush handle
      force <$!> C.hGetContents handle
    parseServers = mapMaybe parseAddServer . lines . C.unpack
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
redFlare :: Address -> IO (Result [(Address, Result Report)])
redFlare addr = do
  eservers <- serversList addr
  case eservers of
    Right servs -> (Right . zip servs) <$> mapConcurrently (serverQuery . incPort) servs
    Left err    -> return (Left err)

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
cube2UniChars = V.fromList "\x00ÀÁÂÃÄÅÆÇ\t\n\x0b\x0c\rÈÉÊËÌÍÎÏÑÒÓÔÕÖ\
  \ØÙÚÛ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\
  \\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ÜÝßàáâãäåæçèéêëìíîïñòóôõöøùúû\
  \üýÿĄąĆćČčĎďĘęĚěĞğİıŁłŃńŇňŐőŒœŘřŚśŞşŠšŤťŮůŰűŸŹźŻżŽžЄБГДЖЗИЙЛПУФЦЧШ\
  \ЩЪЫЬЭЮЯбвгджзийклмнптфцчшщъыьэюяєҐґ"

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
versions = [220, 226, 229, 230]

validateVersion :: Version -> Parser Version
validateVersion v | v `elem` versions = return v
                  | otherwise = fail ("unknown version " ++ show v)

-- | Datatype to represent current state of Red Eclipse server
data Report = Report {
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
gameSpecificMuts n   "deathmatch" | n `elem` [229, 230] = ["gladiator", "oldschool"]
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
gameStates n | n `elem` [229, 230] = ["waiting", "get-map", "send-map", "readying", "game-info", "playing", "overtime", "intermission", "voting"]
gameStates _   = []

parseIf :: Bool -> Parser a -> Parser (Maybe a)
parseIf False _ = return Nothing
parseIf True  p = Just <$> p

-- | Parse server's report.
reReport :: Parser Report
reReport = do
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
  verBranch <- parseIf (version `elem` [229, 230]) reString
  playerNames <- map uncolorString <$> replicateM playerCnt reString
  handles <- parseIf (version /= 220) (replicateM playerCnt reString)
  return Report {..}

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
deriveToJSON defaultOptions { omitNothingFields = True } ''Report
deriveToJSON defaultOptions { omitNothingFields = True } ''VerInfo
