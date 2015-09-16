{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import Data.Word (Word16)
import Data.Aeson (encode, ToJSON(..), object, (.=))
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

data RFArgs = RFArgs { masterServer :: HostName
                     , portNumber :: Word16
                     , showEmpty :: Bool
                     , showErrors :: Bool }
  deriving (Show, Data, Typeable)


rfargs = RFArgs { masterServer = def &= argPos 0 &= opt ("play.redeclipse.net" :: String) &= typ "Host"
                , portNumber = def &= argPos 1 &= opt (28800 :: Word16) &= typ "Port"
                , showEmpty = def &= help "Show empty servers in output"
                , showErrors = def &= help "Show servers that RedFlare failed to connect to" }
           &= summary "RedFlare 0.1.0.0"
           &= details ["Outputs Red Eclipse's servers report from a given master server in JSON."
                      ,"Takes host and port of the master server."
                      ,"Default host is \"play.redeclipse.net\"."
                      ,"Default port is 28800."
                      ,"These correspond to official Red Eclipse master server."]

main :: IO ()
main = do
  args <- cmdArgs rfargs
  reports <- redFlare (masterServer args) (fromIntegral $ portNumber args)
  let reports' = filter (shouldShow args) reports
  LBS.putStrLn $ encode (map Report reports')
  where
    shouldShow (RFArgs { showEmpty = False })
               ((_, _), Right (ServerReport { playerCnt = 0 })) = False
    shouldShow (RFArgs { showErrors = False })
               ((_, _), Left _) = False
    shouldShow _ _ = True

