module Language.PN.Visualize where

import Language.PN.DataType.Types

import qualified Data.Set as S
import System.Directory
import System.Environment
import System.Process
import Control.Exception

import Control.Concurrent
import Data.List

import qualified Debug.Trace as D

import qualified Data.Map.Internal.Debug as D
import qualified Data.Map.Lazy as M


dir = "../visualize"

-- Up to but not including toI
openVisualizations :: Int -> Int -> IO ()
openVisualizations fromI toI | fromI >= toI = return ()
openVisualizations fromI toI
  = do
  let targetName = dir ++ "/pn" ++ show fromI
  --readProcess "chrome" ["--incognito", targetName ++ ".svg"] ""
  callCommand $ "firefox " ++ targetName ++ ".svg & > /dev/null"
      {-  Async call:
  createProcess (CreateProcess {cmdspec = ShellCommand ("chrome --incognito " ++ targetName ++ ".svg"),
                                cwd= Nothing,
                                env =Nothing,
                                std_in = NoStream,
                                std_out = NoStream,
                                std_err = NoStream,
                                close_fds = True,
                                create_group = True,
                                create_new_console = False,
                                delegate_ctlc = True,
                                detach_console = True,
                                new_session = True,
                                child_group = Nothing,
                                child_user = Nothing
                                })
  threadDelay (1000*200)
  -}
  openVisualizations (fromI + 1) toI

compileDot :: String -> IO ()
compileDot targetName
  = callCommand $ "dot -Tsvg " ++ targetName ++ ".dot -o " ++ targetName ++ ".svg"

visualizationNum :: IO Int
visualizationNum
  = length . filter (isSuffixOf ".dot") <$> listDirectory dir

visualizePN :: Show a => PN a -> IO ()
visualizePN pn
  = do
  n <- visualizationNum
  let targetName = dir ++ "/pn" ++ show n
      graphName = ""
      dotCode = dotHeader graphName ++
                concatMap (\p -> dotPlace pn p (show p)) (pn_places pn) ++
                concatMap dotTransition (pn_transitions pn) ++
                dotFooter
  writeFile (targetName ++ ".dot") dotCode
  compileDot targetName

{-
visualizePNConfs :: PNSLConf -> [Action] -> IO ()
visualizePNConfs pnConf tr
  = do
  n <- visualizationNum
  let targetName = dir ++ "/pn" ++ show n
      graphName = "pn" ++ show n ++
                  "  tr: " ++ intercalate " <- " (map show $ take 5 tr) ++ (if length tr > 5 then " <- ..." else "")
      dotCode = dotHeader graphName ++
                labelPlaces pnConf ++
                ppPN pnConf ++
                confs2dot False (S.elems $ confs pnConf) ++
                dotFooter
  writeFile (targetName ++ ".dot") dotCode
  compileDot targetName
-}


strip :: Eq a => a -> [a] -> [a]
strip x xs = filter (/= x) xs


replace :: Eq a => (a, [a]) -> [a] -> [a]
replace (x,x') xs
  = concatMap (\y -> if x == y then x' else [y]) xs

replaces :: Eq a => [(a, [a])] -> [a] -> [a]
replaces rs xs
  = foldr replace xs rs

fontsize = "12.0"

dotPlace :: PN a -> Place -> String -> String
dotPlace pn p lbl
  = let isStart = any (== p) (pn_inits pn)
        isExit  = any (== p) (pn_exits pn)
        lbl' = (if isStart then "Start: " else "") ++
               (if isExit  then "End: " else "") ++
               "p(" ++ lbl ++ ")"
    in show p ++ " [label=\"" ++ strip '\"' lbl' ++ "\"];\n"


hor = "" -- "\\ \\ \\ \\ \\ \\ \\ \\ "
ppEvent :: Show evTy => evTy -> String
ppEvent ev
  = let evStr = replaces [('\n', "\\n"),
                          ('{', "\\{"),
                          ('}', "\\}")]
                          --('_', "\\\\_"),
              $ show ev
    in show ev -- "\\$" ++ hor ++ evStr ++ hor ++ "\\$"

vert = "\\n\\n"
dotTransition :: Show evTy => Transition evTy -> String
dotTransition (Transition tid ev froms tos)
  = show tid ++ " [shape=box, label=\"" ++ hor ++ "t(" ++ show tid ++ "\\): " ++
    strip '\"' (ppEvent ev) ++ "\"];\n" ++
    concatMap (flip arrowDot tid) froms ++
    concatMap (arrowDot tid) tos
  where arrowDot p p' = show p ++ " -> " ++ show p' ++ ";\n"

dotHeader :: String -> String
dotHeader graphName
  = "digraph {\nlabel=\"" ++ strip '\"' graphName ++ "\"\n"

dotFooter :: String
dotFooter
  = "}\n"

--
-- Dedicated pnconf visualization
--

{-
labelPlace :: PNSLConf -> Place -> String
labelPlace pnConf p
  | null $ confs pnConf
    = show p
  | otherwise
    = let markersHere = map length
                      $ map (filter (== p))
                      $ (map . map) m_place
                      $ map c_markers (S.elems $ confs pnConf)
          soundNum    = minimum markersHere
          unsoundNum  = maximum markersHere
          markerDot   = take soundNum (repeat '.')
                        ++ take (unsoundNum - soundNum) (repeat ',')
      in show p ++ markerDot

labelPlaces :: PNSLConf -> String
labelPlaces pnConf
  = concatMap (\p -> dotPlace (protocol pnConf) p (labelPlace pnConf p)) (pn_places $ protocol pnConf)

ppCtrlState :: Queue -> String
ppCtrlState q = intercalate " <- " $ map show (map fst q)

ppPN :: PNSLConf -> String
ppPN pnConf
  = concatMap dotTransition (pn_transitions $ protocol pnConf)

ppMarker :: Marker -> String
ppMarker m = show (m_place m)

fixc :: Char -> String
fixc '"' = "\\\""
fixc '\n' = "\\l"
fixc c = [c]

fixstr :: String -> String
fixstr
  = concatMap fixc

confs2dot :: Bool -> [ConfM] -> String
confs2dot dumpEnvs confs
  = let confs' = zip confs [0..]
    in "node [shape=plaintext, style=solid, width=3.5]\n"
       ++ "subgraph States {\nlabel=\"States\";\n"
       ++ "____k1 [label=\"" ++ intercalate "---\\l" (map conf2dot confs')
       ++ "\"]}\n"
  where conf2dot :: (ConfM, Int) -> String
        conf2dot ((ConfM pid ms q), i) = show i ++ "(" ++ fixstr (show pid) ++ "): [" ++ intercalate "," (map ppMarker ms) ++ "], "
                                     ++ ppCtrlState q ++ "\\l"
                                     ++ if dumpEnvs
                                          then concatMap listMarkerEnv (zip ms [0..])
                                          else []
        listMarkerEnv :: (Marker, Int) -> String
        listMarkerEnv (m, mI) = "env " ++ show mI ++ "p(" ++ show (m_place m) ++ ")"
                                ++ fixstr (D.showTree (m_env m))

-}
