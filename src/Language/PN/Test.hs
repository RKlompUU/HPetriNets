module Language.PN.Test where

import Language.PN.EDSL
import Language.PN.Visualize
import Language.PN.DataType.Types



printTest :: IO ()
printTest
  = do
  let pn = buildPN testPN
  --let pn = buildPN thesisPN
  i <- visualizationNum
  visualizePN pn
  visualizePN (optimizePN pn)
  openVisualizations i (i+2)

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

control
  = loop $ do
      transition "Request stuff?"
      transition "Received stuff!"

pulseForth
  = loop $ do
      transition "Server still alive?"
      transition "Server still alive!"

pulseBack
  = loop $ do
      transition "Client still alive!"
      transition "Client still alive?"


testPN
  = do
  loop $ parallel [p1,
                   optional $ transition "Optional!" ] Nothing

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
