{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification #-}
module Language.PN.EDSL (
    PNBuilder,
    transitionTo,
    transition,
    continue,
    choice,
    choices,
    nop,
    fork,
    forks,
    label,
    catch,
    join,
    wait,
    parallel,
    loop,
    while,
    optional,
    markInit,
    markExit,
    labelTail,
    buildPN,
    optimizePN
  ) where

import Language.PN.DataType.Types
import Data.Foldable
import Control.Monad.State hiding (join)
import Control.Applicative hiding (optional)
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.List

type Ident = String


--
-- The four basic construction functions
--

transitionTo :: evTy -> Pointer -> PNBuilder evTy ()
transitionTo ev p' =
  transitionTo' (Just ev) p'

transitionTo' :: Maybe evTy -> Pointer -> PNBuilder evTy ()
transitionTo' ev p'
  = do
  addTrans [Tail] ev [p']
  setTail p'

choice :: PNBuilder evTy () -> PNBuilder evTy () -> PNBuilder evTy ()
choice pnA pnB
  = do
  t   <- evalPoint Tail
  t'  <- evalPoint NewPlace

  pnA >> transitionTo' Nothing t'
  setTail t
  pnB >> transitionTo' Nothing t'

fork :: PNBuilder evTy () -> PNBuilder evTy ()
fork pn
  = do
  p   <- evalPoint NewPlace
  t'  <- evalPoint NewPlace

  addTrans [Tail] Nothing [p,t']
  setTail p
  pn
  setTail t'

join :: [Pointer] -> Maybe evTy -> PNBuilder evTy ()
join ps ev
  = do
  t' <- evalPoint NewPlace

  addTrans (Tail:ps) ev [t']
  setTail t'


--
-- Convenience variations
--

-- No operation
nop :: PNBuilder evTy ()
nop = return ()

transition :: evTy -> PNBuilder evTy ()
transition ev
  = evalPoint NewPlace >>= transitionTo ev

continue :: evTy -> PNBuilder evTy ()
continue ev
  = transitionTo ev Tail

optional :: PNBuilder evTy () -> PNBuilder evTy ()
optional pn
  = choice pn nop

choices :: [PNBuilder evTy ()] -> PNBuilder evTy ()
choices []
  = nop
choices pns
  = foldr1 choice pns

forks :: [PNBuilder evTy ()] -> PNBuilder evTy ()
forks pns
  = mapM_ fork pns

wait :: [Pointer] -> PNBuilder evTy ()
wait ps
  = join ps Nothing

--
-- Build patterns
--

loop :: PNBuilder evTy () -> PNBuilder evTy ()
loop pn
  = do
  t <- evalPoint Tail

  pn
  transitionTo' Nothing t -- This already restores the tail to p :)

while :: evTy -> PNBuilder evTy () -> PNBuilder evTy ()
while ev pn
  = loop (transition ev >> pn)

parallel :: [PNBuilder evTy ()] -> Maybe evTy -> PNBuilder evTy ()
parallel (pn:pns) joinEv
  = do
  pnsHeads <- replicateM (length pns) (evalPoint NewPlace)
  let pns' = map (\(pn_, p_) -> pn_ >> transitionTo' Nothing p_) (zip pns pnsHeads)

  forks pns'
  pn
  join pnsHeads joinEv

catch :: Maybe evTy -> Pointer -> PNBuilder evTy () -> PNBuilder evTy ()
catch ev p' pn = do
  pushCatch
  pn
  ps <- popCatch
  p'' <- evalPoint p'
  mapM_ (\p -> addTrans [p] ev [p'']) ps

--
-- State business
--

data PNBuilderState evTy =
  BuildState {
    st_pCounter :: Int, -- New place counter
    st_lCounter :: Int, -- Label counter
    st_tail :: Int,
    st_lbls :: Either (M.Map Ident Place) (M.Map Ident Place),
    st_pn   :: PN evTy,
    st_catching :: [[Pointer]]
  }
initPNBuilderState =
  BuildState {
    st_pCounter = 1,
    st_lCounter = -1,
    st_tail = 0,
    st_lbls = Left M.empty,
    st_pn = PN [] [] [],
    st_catching = []
  }
type PNBuilder evTy a = State (PNBuilderState evTy) a


class Pointer_ p where
  unpoint :: p -> PNBuilder evTy Place

instance Pointer_ Int where
  unpoint placeID = return placeID

instance Pointer_ String where
  unpoint = findLabel

data Pointer = forall a. Pointer_ a => P a
             | Tail
             | NewPlace

label :: String -> Pointer
label = P

instance Pointer_ Pointer where
  unpoint Tail     = st_tail <$> get
  unpoint NewPlace = newPlace >>= unpoint
  unpoint (P p)    = unpoint p

evalPoint :: Pointer -> PNBuilder evTy Pointer
evalPoint p = P <$> unpoint p

buildPN :: (Show evTy, Eq evTy) => PNBuilder evTy () -> PN evTy
buildPN builder
  = let st   = execState builder initPNBuilderState
        lbls = takeLeft (st_lbls st)
        st'  = execState builder (initPNBuilderState {st_lbls = Right lbls, st_pCounter = st_pCounter st})
        pn' = (st_pn st') {pn_transitions = reverse $ assignTransIDs (pn_transitions (st_pn st')) (st_pCounter st')}
    in pn'
  where takeLeft :: Either l r -> l
        takeLeft e = case e of
                       Left l -> l
                       _      -> undefined

assignTransIDs ts i =
  map (\(t,id) -> t {t_id = id})
      (zip ts [i..])
optimizePN :: Eq evTy => PN evTy -> PN evTy
optimizePN pn
  = let pn' = nubTrans $ purgethetas pn
        highestID = maximum
                  $ map t_id (pn_transitions pn)
    in pn' {pn_transitions = assignTransIDs (pn_transitions pn') highestID}

nubTrans :: Eq evTy => PN evTy -> PN evTy
nubTrans pn =
  let trans' = foldl addUniqueTrans [] (pn_transitions pn)
  in pn {pn_transitions = trans'}
  where addUniqueTrans ts t =
          if any (sameT t) ts
            then ts
            else t : ts
        sameT t1 t2 = t_entries t1 == t_entries t2 &&
                      t_ev t1 == t_ev t2 &&
                      t_exits t1 == t_exits t2

purgethetas :: PN evTy -> PN evTy
purgethetas pn
  = let ts = pn_transitions pn
        -- Filter out theta transitions that directly loop back to their sources
        ts' = filter (\(Transition _ ev srcs dests) -> (not $ isTheta ev) || srcs /= dests) ts
        ps = filter (\p -> let entries = transitionsToPlace pn p
                               exits   = transitionsFromPlace pn p
                           in (not . null) exits && (not . null) entries &&
                              and (map (\t -> isTheta (t_ev t) &&
                                              length (t_entries t) == 1)
                                       exits))
           $ filter (\p -> not $ any (== p) (pn_inits pn) ||
                                 any (== p) (pn_exits pn))
           $ pn_places pn
        pn'  = pn {pn_transitions = ts'}
        pn'' = foldr purgePlace pn' ps
    in pn''
  where purgePlace p pn_ = let tsPurge = transitionsFromPlace pn_ p
                               moveToPs = map t_exits tsPurge
                               moveTs  = transitionsToPlace pn_ p
                               newExits t ps = filter (/= p) (t_exits t) ++ ps
                               movedTs = concatMap (\t -> map (\ps -> t {t_exits = newExits t ps}) moveToPs) moveTs
                           in pn_ {pn_transitions = (((pn_transitions pn_) \\ moveTs) \\ tsPurge) ++ movedTs}

markInit :: PNBuilder evTy ()
markInit = do
  p <- unpoint Tail
  st <- get
  let pn  = st_pn st
      pn' = pn {pn_inits = p : pn_inits pn}
  put $ st {st_pn = pn'}

markExit :: PNBuilder evTy ()
markExit = do
  p <- unpoint Tail
  st <- get
  let pn  = st_pn st
      pn' = pn {pn_exits = p : pn_exits pn}
  put $ st {st_pn = pn'}

pushCatch :: PNBuilder evTy ()
pushCatch = do
  st <- get
  let catching = st_catching st
  put $ st {st_catching = [] : catching}

popCatch :: PNBuilder evTy [Pointer]
popCatch = do
  st <- get
  let catching = st_catching st
  put $ st {st_catching = tail catching}
  return $ head catching

addLabel :: Ident -> Pointer -> PNBuilder evTy ()
addLabel lbl p = do
  p' <- unpoint p
  st <- get
  case st_lbls st of
    Left m -> let m' = if M.notMember lbl m
                        then M.insert lbl p' m
                        else trace ("Can't redefine already existing label! " ++ lbl) m
              in put $ st {st_lbls = Left m'}
    Right m -> return ()

findLabel :: Ident -> PNBuilder evTy Place
findLabel lbl = do
  lbls <- st_lbls <$> get
  case lbls of
    Left m -> if M.notMember lbl m
                then do
                  addLabel lbl NewPlace
                  findLabel lbl
                else return $ m M.! lbl
    Right m -> return $ m M.! lbl

labeledP :: Pointer -> PNBuilder evTy Pointer
labeledP p = do
  lbl <- newLabel
  addLabel lbl p
  return (P lbl)

newLabel :: PNBuilder evTy Ident
newLabel = do
  st <- get
  let l = st_lCounter st
  put $ st {st_lCounter = l-1}
  return (show l)

newPlace :: PNBuilder evTy Pointer
newPlace = do
  st <- get
  let id = st_pCounter st
  put $ st {st_pCounter = id + 1}
  return (P id)

setTail :: Pointer -> PNBuilder evTy ()
setTail p
  = do
  t  <- evalPoint Tail
  p' <- unpoint p
  st <- get
  let catching' = map (\catcher -> t : catcher) (st_catching st)
  put $ st {st_tail = p', st_catching = catching'}

addTrans :: [Pointer] -> Maybe evTy -> [Pointer] -> PNBuilder evTy ()
addTrans srcs ev dests
  = do
  srcs'  <- mapM unpoint srcs
  dests' <- mapM unpoint dests
  st <- get
  let pn  = st_pn st
      pn' = pn {pn_transitions = Transition 0 ev srcs' dests' : pn_transitions pn}
  put $ st {st_pn = pn'}

labelTail :: Ident -> PNBuilder evTy ()
labelTail lbl
  = addLabel lbl Tail



