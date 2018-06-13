module Main where

import System.Exit

import Language.PN.EDSL
import Language.PN.Visualize
import Language.PN.DataType.Types
import System.IO
import Control.Monad

main :: IO ()
main = do
  printTest "tests/genviz/testPN" testPN
  printTest "tests/genviz/thesisPN" thesisPN
  printTest "tests/genviz/wsPN" wsPNBuilder

  checkFilesEqual "tests/referenceResults/testPN-optimized.dot"
                  "tests/genviz/testPN-optimized.dot"
  checkFilesEqual "tests/referenceResults/thesisPN-optimized.dot"
                  "tests/genviz/thesisPN-optimized.dot"
  checkFilesEqual "tests/referenceResults/wsPN-optimized.dot"
                  "tests/genviz/wsPN-optimized.dot"

checkFilesEqual :: FilePath -> FilePath -> IO ()
checkFilesEqual fA fB = do
  hA <- openFile fA ReadMode
  hB <- openFile fB ReadMode
  cA <- hGetContents hA
  cB <- hGetContents hB
  when (not $ cA == cB) $ putStrLn ("dot file changed! " ++ show fA) >> exitFailure
  hClose hA
  hClose hB

printTest :: String -> PNBuilder String () -> IO ()
printTest name builder
  = do
  let pn = buildPN builder
  visualizePN pn (name ++ "-unoptimized")
  visualizePN (optimizePN pn) (name ++ "-optimized")

thesisPN :: PNBuilder String ()
thesisPN
  = do
  markInit
  transition "Login?"
  while "No!" $ do
    transition "Login?"
  transition "Logged in!"
  forks [control, pulseForth, pulseBack]
  transition "Logout?"
  transition "Logged out!"
  markExit

control :: PNBuilder String ()
control
  = loop $ do
      transition "Request stuff?"
      transition "Received stuff!"

pulseForth :: PNBuilder String ()
pulseForth
  = loop $ do
      transition "Server still alive?"
      transition "Server still alive!"

pulseBack :: PNBuilder String ()
pulseBack
  = loop $ do
      transition "Client still alive!"
      transition "Client still alive?"

testPN :: PNBuilder String ()
testPN
  = do
  loop $ parallel [p1,
                   optional $ transition "Optional!" ] Nothing

p1 :: PNBuilder String ()
p1 = do
  loop $ do
    transition "First!"
    choice (transition "MaybeSecondA!")
           (transition "MaybeSecondB!" )
    choices [transition "CsA!"
            ,transition "CsB!"
            ,transition "CsC!"
            ,transition "CsD!"]
  transition "LeaveLoop!"


wsPNBuilder = do
  markInit
  transition "ClientOpenHandshake?"
  transition "ServerOpenHandshake!"
  parallel [cli2serv,
            serv2cli]
           Nothing
  markExit

cycleEnd = "400"

cli2serv = do
  parallel [cliPing,
            cliData]
           (Just $ "MaskedClose? `cycle > " ++ cycleEnd ++ "`")

serv2cli = do
  parallel [servPing,
            servData]
           (Just "Close!")

cliData = do
  loop $ do
    catch Nothing (label "closeCliData") $ do
      continue "MaskedMessageFrame?"
      transition "MaskedMessageStartFrame?"
      continue "MaskedContinuationFrame?"
      transition "MaskedMessageEndFrame?"
  transitionTo' Nothing (label "closeCliData")

servData = do
  loop $ do
    catch Nothing (label "closeServData") $ do
      continue "MessageFrame!"
      transition "MessageStartFrame!"
      continue "ContinuationFrame!"
      transition "MessageEndFrame!"
  transitionTo' Nothing (label "closeServData")

cliPing = do
  loop $ do
    transition ("MaskedPing? `cycle <= " ++ cycleEnd ++ "` {payload = msg.frame.payload_data}")
    transition "Pong! [payload == msg.frame.payload_data]"

servPing = do
  loop $ do
    transition "Ping! {payload = msg.frame.payload_data}"
    transition "MaskedPong? [payload == msg.frame.payload_data]"
