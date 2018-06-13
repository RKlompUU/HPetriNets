{-# LANGUAGE RecordWildCards #-}
module Language.PN.DataType.Types where

import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import qualified Debug.Trace as D


type Place = Int

data Transition evTy = Transition {
  t_id      :: Int,
  t_ev      :: Maybe evTy,
  t_entries :: [Place],
  t_exits   :: [Place]
}

isTheta :: Maybe evTy -> Bool
isTheta Nothing = True
isTheta _       = False

instance Eq (Transition evTy) where
  t1 == t2 = t_id t1 == t_id t2

instance Functor Transition where
  fmap f t = t {t_ev = f <$> t_ev t}

instance Show evTy => Show (Transition evTy) where
  show (Transition id ev entries exits)
    = "Transition(" ++ show id ++ ") " ++
      show entries ++ "  --" ++ (if isJust ev then show (fromJust ev) else "") ++ ">  " ++
      show exits

showTrans t = show $ fmap (const "") t
showTranss ts = show $ map (fmap (const "")) ts

data PN evTy = PN {
  pn_transitions  :: [Transition evTy],
  pn_inits :: [Place],
  pn_exits :: [Place]
} deriving (Eq)

pn_places :: PN evTy -> [Place]
pn_places pn
  = nub $ concatMap (\t -> t_entries t ++ t_exits t) (pn_transitions pn)

pn_events :: Eq evTy => PN evTy -> [evTy]
pn_events pn
  = nub $ mapMaybe t_ev $ pn_transitions pn

instance Functor PN where
  fmap f pn = pn {pn_transitions = map (fmap f) $ pn_transitions pn}

instance Show evTy => Show (PN evTy) where
  show (PN ts _ _)
    = intercalate "\n"
    $ map show ts

isExitPlace :: PN evTy -> Place -> Bool
isExitPlace pn p = any (== p) (pn_exits pn)

transitionsToPlace :: PN evTy -> Place -> [Transition evTy]
transitionsToPlace pn p
  = filter (any (== p) . t_exits) (pn_transitions pn)

transitionsFromPlace :: PN evTy -> Place -> [Transition evTy]
transitionsFromPlace pn p
  = filter (any (== p) . t_entries) (pn_transitions pn)




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-

-- | Actions are either internal (tau, might have or might not have been
-- traversed by the SUT at any time), always traversable (theta, used for
-- modeling purposes), observable or controllable events.
data Action = Control M.MessageLabel
            | Observe M.MessageLabel
            | Tau
            | Theta
  deriving (Eq, Ord)

type Guard = Maybe E.Expression

data Statement = Statement {
  stmt_v :: String,
  stmt_e :: E.Expression
} deriving (Eq)

data EventTy = EventTy {
  e_action :: Action,
  e_testerguard :: Guard,
  e_guard  :: Guard,
  e_stmts  :: [Statement]
} deriving (Eq)

tauEvent = EventTy Tau Nothing Nothing []
thetaEvent = EventTy Tau Nothing Nothing []

instance Show Statement where
  show ss = stmt_v ss ++ " = " ++ show (stmt_e ss)

instance Show EventTy where
  show ev = show (e_action ev) ++
            (case e_testerguard ev of
              Just g -> "\n`" ++ show g ++ "`"
              Nothing -> "") ++
            (case e_guard ev of
              Just g -> "\n[" ++ show g ++ "]"
              Nothing -> "") ++
            (case e_stmts ev of
              [] -> ""
              ss -> "\n{" ++ intercalate ", " (map show ss) ++ "}")

type PNSL = PN EventTy

getMessageLabel :: Action -> M.MessageLabel
getMessageLabel (Observe m) = m
getMessageLabel (Control m) = m

isObserveAction :: Action -> Bool
isObserveAction (Observe _) = True
isObserveAction _ = False

isControlAction :: Action -> Bool
isControlAction (Control _) = True
isControlAction _ = False

isTauAction :: Action -> Bool
isTauAction Tau = True
isTauAction _   = False

isThetaAction :: Action -> Bool
isThetaAction Theta = True
isThetaAction _     = False

instance Show Action where
  show (Control msg) = msg ++ "?"
  show (Observe msg) = msg ++ "!"
  show Tau           = "tau"
  show Theta         = "theta"

type Trace = [Action]

type Queue = [(Action, M.MessageInstance)]

data Marker = Marker {
  m_place :: Place,
  m_env   :: E.Env
}
type Marking = [Marker]

instance Eq Marker where
  m1 == m2 = m_place m1 == m_place m2

instance Ord Marker where
  compare m1 m2 = compare (m_place m1) (m_place m2)

instance Show Marker where
  show (Marker p _)
    = "Marker: " ++ show p


data ConfM = ConfM {
  c_pid     :: LTS.StateID, -- State ID in execution model (the ProbLTS)
  c_markers :: Marking,
  c_queue   :: Queue
}

instance Eq ConfM where
  c1 == c2 = c_markers c1 == c_markers c2 &&
             map fst (c_queue c1) == map fst (c_queue c2)
instance Ord ConfM where
  compare c1 c2 = compare (c_markers c1) (c_markers c2)
instance Show ConfM where
  show c = show (c_markers c) ++ " " ++ show (map fst (c_queue c))

queueEv :: ConfM -> (Action, M.MessageInstance) -> ConfM
queueEv confM (ev,msg)
  = ConfM (c_pid confM) (c_markers confM) ((ev,msg) : c_queue confM)

unqueueTrace :: Trace -> ConfM -> ConfM
unqueueTrace t (ConfM pid ms q)
  = ConfM pid ms (deleteFirstsBy (\e1 e2 -> fst e1 == fst e2) q (zip t (repeat undefined)))

data PNMeta = PNMeta {
  pnm_async      :: M.Map Place [Action]
} deriving Show

data PNSLConf = PNSLConf {
  protocol     :: PNSL,
  meta         :: PNMeta,
  confs        :: S.Set ConfM
}

instance Show PNSLConf where
  show (PNSLConf pn pnm cMs)
    = "PNConf {\n" ++ show pn ++ ",\ncM: " ++
      intercalate "\ncM: " (map show $ S.elems cMs) ++
      "\n********** Meta data *********\n" ++ show pnm

-- Computes traces of marker paths (p_i ~~~> p_j),
-- up to 1 iteration of loops
-- See [1]
markerPaths :: PNSL -> Place -> [[Transition EventTy]]
markerPaths pn i =
  markerPaths' pn i []

-- Continuation traces of a marking
--
-- Finite length. Up to 1 iteration of loops
--
-- An over-estimation. See [1], page 13 and page 14
contTraces :: PNSL -> [Place] -> [[Action]]
contTraces pn ps =
  nub
    $ filter (not . null)
    $ map (filter (\a -> a /= Theta && a /= Tau))
    $ map (\ts -> map (e_action . t_ev) ts)
    $ concatMap (\p -> markerPaths pn p) ps

markerPaths' :: PNSL -> Place -> [Transition EventTy] -> [[Transition EventTy]]
markerPaths' pn i vs =
  let ts   = filter (flip notElem vs)
           $ transitionsFromPlace pn i
      ps'  = concatMap (\t -> map (\j -> ([t], j))
                              (t_exits t))
                       ts
  in if null ts
       then [[]]
       else concatMap (\([t],i') -> let paths = markerPaths' pn i' (t:vs)
                                    in map ((:) t) paths)
                      ps'


type PNAnalyzer a = Analyzer [Place] (Int,Action) a

pushGlobalStmt :: PNSLConf -> Statement -> PNSLConf
pushGlobalStmt pnsl stmt =
  let confMs' = S.map (\cM -> cM { c_markers = map (\m -> m { m_env = evalStatement E.emptyEnv (m_env m) stmt })
                                             $ c_markers cM })
              $ confs pnsl
  in pnsl { confs = confMs' }


evalStatements :: E.Env -> E.Env -> [Statement] -> E.Env
evalStatements msgEnv markerEnv ss = foldl (evalStatement msgEnv) markerEnv ss
evalStatement :: E.Env -> E.Env -> Statement -> E.Env
evalStatement msgEnv markerEnv s = let e' = case E.evaluate (E.combineEnvs msgEnv markerEnv) (stmt_e s) of
                                                   Left err -> error (show err)
                                                   Right e -> e
                                   in addExpValue2Env (FieldName $ stmt_v s) e' markerEnv

-- Implements M--a \in L-->M'
--
-- Does not consider firing any Tau or Theta transitions. These must be
-- considered by the calling function prior to and after calling this
-- function
fireTransitionsOnEv :: PNSL -> (Action, M.MessageInstance) -> ConfM -> PNAnalyzer [ConfM]
fireTransitionsOnEv pn (ev,msg) confM =
  let ts = filter ((==) ev . e_action . t_ev) (pn_transitions pn)
  in concat <$> mapM (fireTransition confM msg) ts

fireTransition :: ConfM -> M.MessageInstance -> Transition EventTy -> PNAnalyzer [ConfM]
fireTransition (ConfM pid markers q) msg (Transition t_id ev entries exits)
  = let mEntries = map (\p -> filter ((==) p . m_place) markers)
                 $ entries
        firesets = sequence mEntries
        msgEnv   = M.msgEnv msg
    in mapM (fireset msgEnv) firesets
  where fireset :: E.Env -> [Marker] -> PNAnalyzer ConfM
        fireset msgEnv ms = let env' = evalStatements msgEnv (foldr1 E.combineEnvs (map m_env ms)) (e_stmts ev)
                                mExits  = map (flip Marker env') exits
                                c_markers' = mExits ++ filter (\m -> not $ any (== m) ms) markers
                            in do
                              pid' <- registerTransition pid ((t_id,e_action ev)) (sort $ map m_place c_markers')
                              return $ ConfM pid' c_markers' q

-- fireSome implements: s_i (--a-->)^+ s_j
-- (see [1] for the mathematical definition)
--
-- Returns a list of possible configurations after firing a single or more
-- transitions that satisfy the filter argument _f_
fireSome :: PNSL -> (Transition EventTy -> Bool) -> ConfM -> PNAnalyzer [ConfM]
fireSome pn f confM = do
  let ts = filter f
         $ fireableTransitions' pn confM
  confMs  <- concat <$> mapM (fireTransition confM undefined) ts
  confMs' <- concat <$> mapM (fireSome pn f) confMs
  return $ confMs ++ confMs'

-- 0 or more firings of Tau transitions
fireTaus :: PNSL -> ConfM -> PNAnalyzer [ConfM]
fireTaus pn confM =
  let tFilter = (==) Tau . e_action . t_ev
  in ((:) confM) <$> fireSome pn tFilter confM

-- 0 or more firings of Theta transitions that are interleaved by
-- 0 or more firings of Tau transitions, before firing a specific event
-- ev (where ev \in L). fireThetas will not fire ev itself
fireThetas :: PNSL -> Action -> ConfM -> PNAnalyzer [ConfM]
fireThetas pn ev confM =
  let enabledThetas = enableThetas pn ev confM
      tFilter = \t -> e_action (t_ev t) == Tau ||
                      any (== t) enabledThetas
  in ((:) confM) <$> fireSome pn tFilter confM

-- Implementation of: ts = { ... } in [1], Definition 3.
enableThetas :: PNSL -> Action -> ConfM -> [Transition EventTy]
enableThetas pn ev confM =
  let ts = filter (\ts_ -> let evs = map (e_action . t_ev) ts_
                           in all (\a -> a == Theta || a == Tau) (tail evs))
         $ filter (not . null)
         $ map (dropWhile ((/=) ev . e_action . t_ev))
         $ map reverse
         $ concatMap (\m -> markerPaths pn (m_place m)) (c_markers confM)
  in concatMap (filter (\t -> e_action (t_ev t) == Theta)) ts

thetaReach :: PNSL -> ConfM -> [ConfM]
thetaReach pn confM =
  let tFilter = (\a -> a == Theta || a == Tau) . e_action . t_ev
  in confM : analyseLess (fireSome pn tFilter confM)


fireableTransitions :: PNSL -> ConfM -> [Transition EventTy]
fireableTransitions pn confM =
  concatMap (fireableTransitions' pn) (thetaReach pn confM)

-- Without pre tau/theta transitioning
fireableTransitions' :: PNSL -> ConfM -> [Transition EventTy]
fireableTransitions' pn confM =
  filter fitTransition (pn_transitions pn)
  where fitTransition t | all (hasValidMarker (e_testerguard $ t_ev t)) (t_entries t) = True
                        | otherwise = False
        hasValidMarker guard p = any (\m -> m_place m == p && guardPass guard (m_env m)) (c_markers confM)
        guardPass Nothing      _   = True
        guardPass (Just guard) env = case E.evaluate env guard of
                                       Right (E.EBool True) -> True
                                       Left e -> D.trace ("Env: " ++ show env) error $ show e
                                       _ -> False

fireableTransitionsGuard :: PNSL -> ConfM -> E.Env -> [Transition EventTy]
fireableTransitionsGuard pn confM msgsEnv
  = filter fitTransition (pn_transitions pn)
  where fitTransition t | all (hasValidMarker (e_guard $ t_ev t)) (t_entries t) = True
                        | otherwise = False
        hasValidMarker guard p = any (\m -> m_place m == p && guardPass guard (E.combineEnvs (m_env m) msgsEnv)) (c_markers confM)
        guardPass Nothing      _   = True
        guardPass (Just guard) env = case E.evaluate env guard of
                                       Right (E.EBool True) -> True
                                       Left e -> D.trace ("Env: " ++ show env) error $ show e
                                       _ -> False

pnConfFireableTransitions :: PNSLConf -> [Transition EventTy]
pnConfFireableTransitions pnConf
  = concatMap (fireableTransitions $ protocol pnConf) (S.elems $ confs pnConf)

possibleNextEvents :: PNSLConf -> [Action]
possibleNextEvents pnConf
  = map (e_action . t_ev)
  $ pnConfFireableTransitions pnConf

-}
