{-# LANGUAGE OverloadedStrings #-}
module ShellCheck.Formatter.Codeclimate (format) where

import ShellCheck.Interface
import ShellCheck.Formatter.Format

import Control.DeepSeq (deepseq)
import Data.Aeson
import Data.IORef
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Char8 as BS

format :: IO Formatter
format = do
    ref <- newIORef []
    return Formatter
        { header    = return ()
        , onResult  = collectResult ref
        , onFailure = outputError
        , footer    = finish ref
        }

data CCIssue = CCIssue
  { description :: String
  , check_name  :: String
  , fingerprint :: String
  , severity    :: String
  , location    :: CCLocation
  }

data CCLocation = CCLocation
  { path      :: String
  , positions :: CCPositions
  }

data CCPositions = CCPositions
  { begin :: CCPosition
  , end   :: CCPosition
  }

data CCPosition = CCPosition
  { line   :: Integer
  , column :: Integer
  }

-- ToJSON instances
instance ToJSON CCIssue where
    toJSON issue = object
      [ "type"        .= ("issue" :: String)
      , "description" .= description issue
      , "check_name"  .= check_name issue
      , "fingerprint" .= fingerprint issue
      , "severity"    .= severity issue
      , "location"    .= location issue
      ]
    toEncoding issue = pairs
      (  "type"        .= ("issue" :: String)
      <> "description" .= description issue
      <> "check_name"  .= check_name issue
      <> "fingerprint" .= fingerprint issue
      <> "severity"    .= severity issue
      <> "location"    .= location issue
      )

instance ToJSON CCLocation where
    toJSON loc = object
      [ "path"      .= path loc
      , "positions" .= positions loc
      ]

instance ToJSON CCPositions where
    toJSON pos = object
      [ "begin" .= begin pos
      , "end"   .= end pos
      ]

instance ToJSON CCPosition where
    toJSON p = object
      [ "line"   .= line p
      , "column" .= column p
      ]

-- Mapping ShellCheck PositionedComment -> CCIssue
toCCIssue :: PositionedComment -> CCIssue
toCCIssue pc =
    let start        = pcStartPos pc
        endPos       = pcEndPos pc
        filePath     = posFile start
        lineNum      = posLine start
        endLineNum   = posLine endPos
        columnNum    = posColumn start
        endColumnNum = posColumn endPos
        c            = pcComment pc
        codeNum      = cCode c
        msg          = cMessage c
        desc = msg
        checkName = "SC" ++ show codeNum
        fingerprint = filePath ++ ":" ++ show lineNum ++ ":" ++ show codeNum
        sevText = severityText pc
        severityCC = mapSeverity sevText
    in CCIssue
         { description = desc
         , check_name  = checkName
         , fingerprint = fingerprint
         , severity    = severityCC
         , location    = CCLocation filePath (CCPositions (CCPosition lineNum columnNum) (CCPosition endLineNum endColumnNum))
         }

-- ShellCheck severity levels to Code Climate levels
mapSeverity :: String -> String
mapSeverity "error"   = "critical"
mapSeverity "warning" = "major"
mapSeverity "info"    = "minor"
mapSeverity "style"   = "info"
mapSeverity _         = "minor"

outputError :: FilePath -> String -> IO ()
outputError file msg = hPutStrLn stderr $ file ++ ": " ++ msg

collectResult ref cr sys = mapM_ f groups
  where
    commentsAll = crComments cr
    groups = NE.groupWith sourceFile commentsAll
    f :: NE.NonEmpty PositionedComment -> IO ()
    f group = do
      let filename = sourceFile (NE.head group)
      result <- siReadFile sys (Just True) filename
      let contents = either (const "") id result
      let comments' = makeNonVirtual commentsAll contents
      deepseq comments' $ modifyIORef ref (\x -> comments' ++ x)

finish :: IORef [PositionedComment] -> IO ()
finish ref = do
    pcs <- readIORef ref
    let issues = map toCCIssue pcs
    BL.putStrLn $ encode issues

