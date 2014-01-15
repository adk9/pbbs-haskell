{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, RankNTypes #-}

import Data.Set as Set

-- Benchmark utils:
import PBBS.FileReader
import PBBS.Timing (wait_clocks, runAndReport)
-- calibrate, measureFreq, commaint,

import Control.Monad
import Control.Monad.Par
import Control.Monad.Par.Combinator (parFor, InclusiveRange(..))
import Control.Monad.ST
import Control.Exception
import GHC.Conc

import Data.Word
import Data.Maybe
import Data.Time.Clock
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Storable as UV
import qualified Data.Vector.Storable.Mutable as MV
import System.Mem (performGC)
import System.Environment (getArgs)
import System.Directory
import System.Process


-- define DEBUG_CHECKS

--------------------------------------------------------------------------------

-- import Data.LVar.MaxCounter as C
-- #if 1
-- import Data.LVar.PureSet as S
-- #else
-- import Data.LVar.SLSet as S
-- #endif

-- import qualified Data.LVar.SLSet as SL

-- import Data.LVar.NatArray as NArr

-- An LVar-based version of bf_traverse.  As we traverse the graph,
-- the results of applying f to each node accumulate in an LVar, where
-- they are available to other computations, enabling pipelining.
{-
bf_traverse :: Int             -- iteration counter
               -> Graph2       -- graph
               -> ISet WorkRet -- LVar
               -> IS.IntSet    -- set of "seen" node labels, initially size 0
               -> IS.IntSet    -- set of "new" node labels, initially size 1
               -> WorkFn       -- function to be applied to each node
               -> Par (IS.IntSet)
bf_traverse 0 _ _ seen_rank new_rank _ = do
  when verbose $ prnt $ "bf_traverse finished! seen/new size: "
    ++ show (IS.size seen_rank, IS.size new_rank)
  return (IS.union seen_rank new_rank)

bf_traverse k !g !l_acc !seen_rank !new_rank !f = do 
  when verbose $ prnt  $"bf_traverse call... "
    ++ show k ++ " seen/new size "
    ++ show (IS.size seen_rank, IS.size new_rank)
  -- Nothing in the new_rank set means nothing left to traverse.
  if IS.null new_rank
  then return seen_rank
  else do
    -- Add new_rank stuff to the "seen" list
    let seen_rank' = IS.union seen_rank new_rank
        allNbr'    = IS.fold (\i acc -> IS.union (g V.! i) acc) 
                        IS.empty new_rank
        new_rank'  = IS.difference allNbr' seen_rank' 

    -- We COULD use callbacks here, but rather we're modeling what happens in the
    -- current paper:
    parMapM_ (\x -> fork$ do 
              let elem = f x
              S.insert elem l_acc
              when dbg $ do 
                 st <- unsafePeekSet l_acc
                 prnt$ " --> Called S.insert, node "++show x
                      ++" size is "++show(Set.size st) 
                      ++" elem is "++show elem --  ++" "++show st
            )
            (IS.toList new_rank') -- toList is HORRIBLE
    bf_traverse (k-1) g l_acc seen_rank' new_rank' f

start_traverse :: Int       -- iteration counter
                  -> Graph2 -- graph
                  -> Int    -- start node
                  -> WorkFn -- function to be applied to each node
                  -> IO ()
start_traverse k !g startNode f = do
  runParIO $ do        
        prnt $ " * Running on " ++ show numCapabilities ++ " parallel resources..."
        
        l_acc <- newEmptySet
        -- "manually" add startNode
        fork $ S.insert (f startNode) l_acc
        -- pass in { startNode } as the initial "new" set
        set <- bf_traverse k g l_acc IS.empty (IS.singleton startNode) f
        
        prnt $ " * Done with bf_traverse..."
        let size = IS.size set
        
        prnt$ " * Waiting on "++show size++" set results..."

        when dbg $ do 
          forM_ [0..size] $ \ s -> do
            prnt$ " ? Blocking on "++show s++" elements to be in the set..."
            waitForSetSize s l_acc

        -- Waiting is required in any case for correctness, whether or
        -- not we consume the result
        waitForSetSize (size) l_acc -- Depends on a bunch of forked computations
        prnt$ " * Set results all available! (" ++ show size ++ ")"

        s <- consumeSet l_acc :: Par (Set.Set WorkRet)
        liftIO (do evaluate s; return ())
        prnt $ " * Finished consumeSet:"
        prnt $ "  * Set size: " ++ show (Set.size s)
        prnt $ "  * Set sum: " ++ show (Set.fold (\(x,_) y -> x+y) 0 s)

parMapM_ f l =
  do parMapM f l
     return ()
-}


--------------------------------------------------------------------------------
-- Graph algorithms
--------------------------------------------------------------------------------

bfs_async :: AdjacencyGraph -> NodeID -> Par d s (ISet s NodeID)
bfs_async gr@(AdjacencyGraph vvec evec) start = do 
  st <- S.newFromList [start]
  S.forEach st $ \ nd -> do
    logDbgLn 1 $" [bfs] expanding node "++show nd++" to nbrs " ++ show (nbrs gr nd)
    forVec (nbrs gr nd) (`S.insert` st)
  return st
--    T.traverse_ (`S.insert` st) (nbrs gr nd)


-- | A version that uses an array rather than set representation.
bfs_async_arr :: AdjacencyGraph -> NodeID -> Par (IVar Bool)
bfs_async_arr gr@(AdjacencyGraph vvec evec) start = do 
  arr <- new (U.length vvec)
  let callback nd bool = do
       let myNbrs = nbrs gr (fromIntegral nd)        
       logDbgLn 1 $" [bfs] expanding node "++show (nd,bool)++" to nbrs " ++ show myNbrs
       -- TODO: possibly use a better for loop:
       forVec myNbrs (\nbr -> put_ arr (fromIntegral nbr) True)
  ISt.forEachHP Nothing arr callback
  logDbgLn 1 $" [bfs] Seeding with start vertex... "
  ISt.put_ arr (fromIntegral start) True
  return arr

-- | Same, but with NatArray.
bfs_async_arr2 :: AdjacencyGraph -> NodeID -> Par d s (NatArray s Word8)
bfs_async_arr2 gr@(AdjacencyGraph vvec evec) start = do 
  arr <- newNatArray (U.length vvec)
  let callback nd flg = do
       let myNbrs = nbrs gr (fromIntegral nd)        
       -- logDbgLn 1 $" [bfs] expanding node "++show (nd,flg)++" to nbrs " ++ show myNbrs
       forVec myNbrs (\nbr -> NArr.put arr (fromIntegral nbr) 1)
  NArr.forEach arr callback
  -- logDbgLn 1 $" [bfs] Seeding with start vertex... "
  NArr.put arr (fromIntegral start) 1
  return arr

------------------------------------------------------------------------------------------
-- A simple FOLD operation.
------------------------------------------------------------------------------------------  

maxDegreeS :: AdjacencyGraph -> (ISet s NodeID) -> Par d s (MaxCounter s)
maxDegreeS gr component = do
  mc <- newMaxCounter 0 
  S.forEach component $ \ nd ->
    C.put mc (U.length$ nbrs gr nd)
  return mc


maxDegreeN :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (MaxCounter s)
maxDegreeN gr component = do
  mc <- newMaxCounter 0 
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $
      C.put mc (U.length$ nbrs gr (fromIntegral nd))
  return mc


maxDegreeI :: AdjacencyGraph -> (IStructure s Word8) -> Par d s (MaxCounter s)
maxDegreeI gr component = do
  mc <- newMaxCounter 0
  -- INEFFICIENT: this attaches a handler to ALL ivars:
  ISt.forEachHP Nothing component $ \ nd flg -> do
    when (flg == 1) $ do
      let degree = U.length$ nbrs gr (fromIntegral nd)
      -- logDbgLn 1$ " [maxDegreeI] Processing: "++show(nd,flg)++" with degree "++show degree
      C.put mc degree

  -- Better to just do this... wait for it to freeze and then loop.
  -- Problem is, we need to add wait-till-frozen!
  -- len <- ISt.getLength component
  -- parForTiled (0,len) $ \ nd ->
  --   when (flg == 1) $
  --     C.put mc (U.length$ nbrs gr (fromIntegral nd))

  return mc

------------------------------------------------------------------------------------------
-- A dummy per-node operation
------------------------------------------------------------------------------------------  

-- workEachNode :: (NatArray s Word8) -> (Word8 -> Par d s ()) -> Par d s (MaxCounter s)
workEachNode :: Word64 -> (NatArray s Word8) -> Par d s ()
workEachNode clocks component = do
  NArr.forEach component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVec :: Word64 -> UV.Vector Word8 -> Par d s ()
workEachVec clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0,UV.length vec) $ \ ix ->  
    let flg = vec UV.! ix in
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

  -- Sequential version:  
  -- UV.forM_ vec $ \ flg ->
  --   when (flg == 1) $ do
  --     liftIO$ wait_clocks clocks
  --     return ()


workEachNodeI :: Word64 -> (IStructure s Word8) -> Par d s ()
workEachNodeI clocks component = do
  ISt.forEachHP Nothing component $ \ nd flg ->
    when (flg == 1) $ do
      liftIO$ wait_clocks clocks
      return ()

-- After freezing... this is a parallel loop, but doesn't use any monotonic data.
workEachVecMayb :: Word64 -> V.Vector (Maybe Word8) -> Par d s ()
workEachVecMayb clocks vec = do
  np <- liftIO$ getNumCapabilities
  -- for_ (0,UV.length vec) $ \ ix ->    
  -- parForTiled (np*4) (0,UV.length vec) $ \ ix ->
  -- parForSimple (0,UV.length vec) $ \ ix ->
  parForTree (0, V.length vec) $ \ ix ->  
    let flg = vec V.! ix in
    when (flg == Just 1) $ do
      liftIO$ wait_clocks clocks
      return ()

--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

{-# INLINE forVec #-}
-- | Simple for-each loops over vector elements.
forVec :: U.Unbox a => U.Vector a -> (a -> Par d s ()) -> Par d s ()
forVec vec fn = loop 0 
  where
    len = U.length vec
    loop i | i == len = return ()
           | otherwise = fn (U.unsafeIndex vec i) >>
                         loop (i+1)

--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------
  
main = do
  putStrLn "USAGE: ./bfs <version> <topo> <graphSize>"
  putStrLn "USAGE:   Topo must be one of: grid rmat rand chain"
  putStrLn "USAGE:   Version must be one of: "
  putStrLn "USAGE:      bfsS bfsN bfsI"
  
  --------------------------------------------------------------------------------
  args <- getArgs
  let (version,topo,size,wrksize::Double) =
        case args of
          [ver,tp,s,w] -> (ver, tp, read s, read w)
          [ver,tp,s]   -> (ver, tp, read s, 0)
          [ver,tp]     -> (ver, tp,      1000, 0)
          [ver]        -> (ver, "grid",  1000, 0) 
          []           -> ("bfsN","grid",1000, 0)
          oth          -> error "Too many command line args!"
      existD d = do b <- doesDirectoryExist d
                    return$ if b then (Just d) else Nothing
                    
  -- Here's a silly hack to let this executable run from different working directories:
  pbbsdirs <- fmap catMaybes $ mapM existD [ "../pbbs"
                                           , "../../pbbs"
                                           , "../../../pbbs"
                                           , "../../../../pbbs"]
  let pbbsroot = case pbbsdirs of
                   [] -> error "PBBS dir not found!  Is the submodule checked out?"
                   hd:_ -> hd
      datroot = pbbsroot++"/breadthFirstSearch/graphData/data/"
      -- The PBBS Makefile knowns how to build the common graphs:
      buildPBBSdat file = do 
        origdir <- getCurrentDirectory
        setCurrentDirectory datroot  
        b <- doesFileExist file
        unless b $ do
          putStrLn "Input file does not exist!  Building..."
          system$ "make "++file
          return ()
        setCurrentDirectory origdir

  file <- case topo of
           "grid" -> do let f = "3Dgrid_J_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           -- Models social-network graphs:
           "rmat" -> do let f = "rMatGraph_J_5_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           "rand" -> do let f = "randLocalGraph_J_5_"++show size
                        buildPBBSdat f
                        return (datroot ++ f)
           "chain" ->  do let f = "chain_"++show size
                              p = datroot ++ f
                          b <- doesFileExist p
                          unless b $ do
                            putStrLn$"Generating chain graph in "++p
                            system "ghc -threaded gen_chains_graph.hs -o ./gen_chains_graph.exe"
                            system$ "./gen_chains_graph.exe "++show size++" > "++p
                            return ()
                          return p                          
           _        -> error$"Unknown graph topology: "++topo
           
  putStrLn$"Running config: "++show(version,topo,size)
  ------------------------------------------------------------
  wd <- getCurrentDirectory
  putStrLn$ "Working dir: "++wd
  putStrLn$ "Reading file: "++file
  t0 <- getCurrentTime  
  gr <- readAdjacencyGraph file
  t1 <- getCurrentTime
  let numVerts = U.length (vertOffets gr)
  putStrLn$ "graph read ("++show (diffUTCTime t1 t0)++
    "): verts,edges: "++show (numVerts, U.length (allEdges gr))

  putStrLn$ "max vert off "++show (U.foldl1 max (vertOffets gr))
  putStrLn$ "max edge target "++show (U.foldl1 max (allEdges gr))
  t2 <- getCurrentTime
  putStrLn$ "time for those simple folds: "++show (diffUTCTime t2 t1)
  performGC
  -- writeFile "/tmp/debug" (show gr)
  -- putStrLn$ "Dumped parsed graph to /tmp/debug"  


  runAndReport $ \ clocks_per_micro ->
    let amountWork = (round (wrksize * clocks_per_micro)) in
    case version of
      ----------------------------------------
      "bfsS" -> do 
                   putStrLn " ! Version 2: BFS only, with sets "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       -- par2 :: Par d0 s0 ()
                       par2 = do comp <- bfs_async gr 0
                                 waitSize numVerts comp -- A proxy for completeness... assumes fully connected graph.
                                 return comp
                   _ <- runParIO par2
                   -- set:: Snapshot ISet NodeID <- runParIO par2
                   -- let ISetSnap s = set                                          
                   -- putStrLn$ "Connected component, set size "++show (Set.size s)
                   return ()

      ----------------------------------------
      "bfsI" -> do putStrLn " ! Version 3: BFS only, with IStructures "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       par3 :: Par d0 s0 (IStructure s0 Bool)
                       par3 = bfs_async_arr gr 0
                   _ <- runParIO par3
                   return ()

      ----------------------------------------
      "bfsN" -> do putStrLn " ! Version 4: BFS only, with NatArrays "
                   let -- par2 :: Par d0 s0 (ISet s0 NodeID)
                       par4 :: Par d0 s0 (NatArray s0 Word8)
                       par4 = bfs_async_arr2 gr 0
                   _ <- runParIO par4
                   return ()
{-
      ----------------------------------------
      "bfsN_work" -> do
              putStrLn " ! Version 12: BFS and per-vertex work"
              let par :: Par d0 s0 ()
                  par = do natarr <- bfs_async_arr2 gr 0
                           workEachNode amountWork natarr
              _ <- runParIO par
              return ()

      ----------------------------------------
      "bfsN_barrier_work" -> do
              putStrLn " ! Version 13: BFS, barrier, and per-vertex work"
              let -- par :: Par d0 s0 ()
                  par = bfs_async_arr2 gr 0
              NatArraySnap vec <- runParIO par
              runParIO $ workEachVec amountWork vec
              return ()


      ----------------------------------------
      "misI_work" -> do
              putStrLn " ! Version 14: MIS and per-vertex work"
              let par :: Par d0 s0 ()
                  par = do istrct <- maximalIndependentSet2 parForL gr
                           workEachNodeI amountWork istrct
              _ <- runParIO par
              return ()

      ----------------------------------------
      "misI_barrier_work" -> do
              putStrLn " ! Version 15: "
              let -- par :: Par d0 s0 ()
                  par = maximalIndependentSet2 parForL gr 
              IStructSnap vec <- runParIO par
              runParIO $ workEachVecMayb amountWork vec
              return ()

      ----------------------------------------
      "bfsN_misI_work" -> do
              putStrLn " ! Version 16: "
              let par :: Par d0 s0 ()
                  par = do natarr <- bfs_async_arr2 gr 0
                           istrct <- maximalIndependentSet4 gr natarr
                           workEachNodeI amountWork istrct
              _ <- runParIO par
              return ()

      ----------------------------------------
      "bfsN_barrier_misI_work" -> do
              putStrLn " ! Version 17: "
              let par = bfs_async_arr2 gr 0
              NatArraySnap vec <- runParIO par
              let vec2 = maximalIndependentSet3B gr vec -- Sequential
              runParIO $ workEachVec amountWork vec2
              return ()

      ----------------------------------------
      "?" -> do
              putStrLn " ! Version 1: work in progress testing combinations of graph ops..."
              let par1 :: Par d0 s0 (MaxCounter s0, ISet s0 NodeID)
                  par1 = do component <- bfs_async gr 0
                            liftIO$ putStrLn "Got component..."
                            mc <- maxDegreeS gr component    
                            return (mc,component)            
              (maxdeg::Int, set:: Snapshot ISet NodeID) <- runParIO par1
              putStrLn$ "Processing finished, max degree was: "++show maxdeg
              let ISetSnap s = set
              putStrLn$ "Connected component, set size "++show (Set.size s)
-}
      oth -> error$"Unknown benchmark mode "++oth

  putStrLn$ "Done"
  
