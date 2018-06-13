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

  checkFilesEqual "tests/referenceResults/testPN-optimized.dot"
                  "tests/genviz/testPN-optimized.dot"
  checkFilesEqual "tests/referenceResults/thesisPN-optimized.dot"
                  "tests/genviz/thesisPN-optimized.dot"

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
