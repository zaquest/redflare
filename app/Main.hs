{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import Data.Word (Word16)
import Data.Aeson (encode, ToJSON(..), object, (.=))
import Options.Applicative
import Network.RedEclipse.RedFlare

newtype Report = Report ((HostName, PortNumber), Either String ServerReport)

instance ToJSON Report where
  toJSON (Report ((host, port), report)) =
    case report of
      Left err -> object [ "host"    .= host
                         , "port"    .= port
                         , "status"  .= ("error" :: Text)
                         , "message" .= err ]
      Right sr -> object [ "host"   .= host
                         , "port"   .= port
                         , "status" .= ("success" :: Text)
                         , "report" .= sr ]

data RFArgs = Master MasterArgs
            | Single SingleArgs

data MasterArgs = MasterArgs { masterHost :: HostName
                             , masterPort :: Word16
                             , showEmpty  :: Bool
                             , showFailed :: Bool }

data SingleArgs = SingleArgs { singleHost :: HostName
                             , singlePort :: Word16 }

args :: ParserInfo RFArgs
args =
  info (helper <*> subparser
         (command "master"
           (info masterOptions
             (progDesc "Receive list of connected servers from master server and poll them to get their current state."
             <> fullDesc))
       <> command "single"
           (info singleOptions
             (progDesc "Get current state of a specified server."
               <> fullDesc))))
       (fullDesc
        <> progDesc "Get current state of Red Eclipse game servers."
        <> header "redflare - Red Eclipse JSON command line server browser")
  where
    masterOptions :: Parser RFArgs
    masterOptions = helper <*> (Master <$> (MasterArgs
      <$> strArgument
            (metavar "HOST" <> value "play.redeclipse.net" <> showDefault
             <> help "Master server's host")
      <*> argument auto
            (metavar "PORT" <> value 28800 <> showDefault
             <> help "Master server's port")
      <*> switch
            (long "show-empty" <> short 'e'
             <> help "If passed output will include reports from empty servers")
      <*> switch
            (long "show-failed" <> short 'f'
             <> help "If passed output will include errors for servers that redflare failed to recieve reports from")))
    singleOptions :: Parser RFArgs
    singleOptions = helper <*> (Single <$> (SingleArgs
      <$> strArgument
            (metavar "HOST"
             <> help "Server's host.")
      <*> argument auto
            (metavar "PORT" <> value 28801 <> showDefault
             <> help "Server's port.")))


run :: RFArgs -> IO ()
run (Master args) = do
  reports <- redFlare (masterHost args) (fromIntegral $ masterPort args)
  let reports' = filter (shouldShow args) reports
  LBS.putStrLn $ encode (map Report reports')
  where
    shouldShow (MasterArgs { showEmpty = False })
                ((_, _), Right (ServerReport { playerCnt = 0 })) = False
    shouldShow (MasterArgs { showFailed = False })
                ((_, _), Left _) = False
    shouldShow _ _ = True
run (Single (SingleArgs { singleHost = host , singlePort = port })) = do
  let port' = fromIntegral port
  report <- serverQuery host port'
  LBS.putStrLn . encode $ Report ((host, port'), report)

main :: IO ()
main = execParser args >>= run
