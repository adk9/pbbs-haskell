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

------------------------------------------------------------------------------------------
-- Maximal Independent Set
------------------------------------------------------------------------------------------  

-- Lattice where undecided = bot, and chosen/nbrchosen are disjoint middle states
flag_UNDECIDED :: Word8
flag_CHOSEN    :: Word8
flag_NBRCHOSEN :: Word8
flag_UNDECIDED = 0
flag_CHOSEN    = 1
flag_NBRCHOSEN = 2

{-# INLINE maximalIndependentSet #-}
-- maximalIndependentSet :: ISet s NodeID -> Par d s (ISet s NodeID)  -- Operate on a subgraph
-- maximalIndependentSet :: AdjacencyGraph -> Par d s (ISet s NodeID) -- Operate on a whole graph.
maximalIndependentSet :: ParFor d s -> AdjacencyGraph -> Par d s (NatArray s Word8) -- Operate on a whole graph.
maximalIndependentSet parFor gr@(AdjacencyGraph vvec evec) = do
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr :: NatArray s Word8 <- newNatArray numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          -- logDbgLn 1$ " [MIS]   ... on nbr "++ show i++" of "++show numNbrs
          let nbrInd = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              -- This should never block in a single-thread execution:
              logDbgLn 1 (" [MIS] ! Getting on nbrInd "++show nbrInd)
              nbrFlag <- NArr.get flagsArr (fromIntegral nbrInd)
              logDbgLn 1 (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then NArr.put flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logDbgLn 1 (" [MIS] ! Node chosen: "++show selfInd) >> 
                         NArr.put flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logDbgLn 1$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr

-- | DUPLICATE CODE: IStructure version.
maximalIndependentSet2 :: ParFor d s -> AdjacencyGraph -> Par d s (IStructure s Word8) -- Operate on a whole graph.
maximalIndependentSet2 parFor gr@(AdjacencyGraph vvec evec) = do
  logDbgLn 1$ " [MIS] Beginning maximalIndependentSet / Istructures"
  -- For each vertex, we record whether it is CHOSEN, not chosen, or undecided:
  let numVerts = U.length vvec
  flagsArr <- newIStructure numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          -- logDbgLn 1$ " [MIS]   ... on nbr "++ show i++" of "++show numNbrs
          let nbrInd = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              -- This should never block in a single-thread execution:
              logDbgLn 1 (" [MIS] ! Getting on nbrInd "++show nbrInd)
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              logDbgLn 1 (" [MIS] ! Get completed on nbrInd "++show nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = logDbgLn 1 (" [MIS] ! Node chosen: "++show selfInd) >> 
                         ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
  parFor (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      -- logDbgLn 1$ " [MIS] processing node "++show ndIx++" nbrs "++show nds
      loop (U.length nds) nds ndIx  0
  return flagsArr


-- | Sequential version.
maximalIndependentSet3 :: AdjacencyGraph -> (U.Vector Word8)
maximalIndependentSet3 gr@(AdjacencyGraph vvec evec) = U.create $ do
  let numVerts = U.length vvec
  flagsArr <- M.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- M.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then M.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = M.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> do 
      let nds = nbrs gr (fromIntegral ndIx)
      loop (U.length nds) nds ndIx 0
  return flagsArr

-- | Sequential version on NatArray...
maximalIndependentSet3B :: AdjacencyGraph -> (UV.Vector Word8) -> (UV.Vector Word8)
maximalIndependentSet3B gr@(AdjacencyGraph vvec evec) vec = UV.create $ do
  let numVerts = U.length vvec
  flagsArr <- MV.replicate numVerts 0
  let loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- MV.read flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then MV.write flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = MV.write flagsArr (fromIntegral selfInd) flag_CHOSEN
  for_ (0,numVerts) $ \ ndIx -> 
      when (vec UV.! ndIx == 1) $ do 
        let nds = nbrs gr (fromIntegral ndIx)
        loop (U.length nds) nds ndIx 0
  return flagsArr


-- MIS over a preexisting, filtered subgraph
------------------------------------------------------------
-- Right now this uses an IStructure because it's (temporarily) better at blocking gets:
maximalIndependentSet4 :: AdjacencyGraph -> (NatArray s Word8) -> Par d s (IStructure s Word8)
maximalIndependentSet4 gr@(AdjacencyGraph vvec evec) vertSubset = do
  let numVerts = U.length vvec
  -- Tradeoff: we use storage proportional to the ENTIRE graph.  If the subset is
  -- very small, this is silly and we could use a sparse representation:
  flagsArr <- newIStructure numVerts
  let       
      -- Here's the loop that scans through the neighbors of a node.
      loop !numNbrs !nbrs !selfInd !i 
        | i == numNbrs = thisNodeWins
        | otherwise = do
          let nbrInd   = fromIntegral$ nbrs U.! i -- Find our Nbr's NodeID
              selfInd' = fromIntegral selfInd
          -- If we got to the end of the neighbors below us, then we are NOT disqualified:
          if nbrInd > selfInd
            then thisNodeWins
            else do
              nbrFlag <- ISt.get flagsArr (fromIntegral nbrInd)
              if nbrFlag == flag_CHOSEN
                then ISt.put_ flagsArr selfInd' flag_NBRCHOSEN
                else loop numNbrs nbrs selfInd (i+1)
        where
          thisNodeWins = ISt.put_ flagsArr (fromIntegral selfInd) flag_CHOSEN
          
  NArr.forEach vertSubset $ \ ndIx _ -> 
        let nds = nbrs gr (fromIntegral ndIx) in
        loop (U.length nds) nds ndIx  0
  return flagsArr

type ParFor d s = (Int,Int) -> (Int -> Par d s ()) -> Par d s ()

--------------------------------------------------------------------------------
-- Main Program
--------------------------------------------------------------------------------
  
main = do
  putStrLn "USAGE: ./mis <version> <topo> <graphSize>"
  putStrLn "USAGE:   Topo must be one of: grid rmat rand chain"
  putStrLn "USAGE:   Version must be one of: "
  putStrLn "USAGE:      misN1 misN2 misN3 misI3 misSeq" 
  
  --------------------------------------------------------------------------------
  args <- getArgs
  let (version,topo,size,wrksize::Double) =
        case args of
          [ver,tp,s,w] -> (ver, tp, read s, read w)
          [ver,tp,s]   -> (ver, tp, read s, 0)
          [ver,tp]     -> (ver, tp,      1000, 0)
          [ver]        -> (ver, "grid",  1000, 0) 
          []           -> ("misN1","grid",1000, 0)
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
      "misN1" -> do 
              putStrLn " ! Version 5: MIS only, with NatArrays / parForSimple"
              let par :: Par d0 s0 (NatArray s0 Word8)
                  par = maximalIndependentSet parForSimple gr
#ifdef DEBUG_CHECKS
              NatArraySnap (x :: UV.Vector Word8) <- runParIO par
              putStrLn$ "MIS: result prefix: "++show (UV.take 100 x)
              putStrLn$ "MIS: number of vertices in result: "++show (UV.sum (UV.filter (==1) x))
#else
              _ <- runParIO par
#endif
              return ()

      ----------------------------------------
      "misN2" -> do 
              putStrLn " ! Version 6: MIS only, with NatArrays / parForTree"
              let par :: Par d0 s0 (NatArray s0 Word8)
                  par = maximalIndependentSet parForTree gr
              _ <- runParIO par
              return ()

      ----------------------------------------
      "misN3" -> do 
              putStrLn " ! Version 7: MIS only, with NatArrays / parForL"
              let par :: Par d0 s0 (NatArray s0 Word8)
                  par = maximalIndependentSet parForL gr
              _ <- runParIO par
              return ()

      ----------------------------------------
      "misI3" -> do 
              putStrLn " ! Version 8: MIS only, with IStructures / parForL"
              let par :: Par d0 s0 (IStructure s0 Word8)
                  par = maximalIndependentSet2 parForL gr
              _ <- runParIO par
              return ()
      -- This version doesn't get the horrible parallel slowdown of version 5-7.
      -- But alas, version 7 sequential is better.
      -- And version 9 sequential is WAY better (>50X faster)

      ----------------------------------------
      "misSeq" -> do 
              putStrLn " ! Version 9: MIS only, sequential"
              evaluate $ maximalIndependentSet3 gr
              return ()

      oth -> error$"Unknown benchmark mode "++oth

  putStrLn$ "Done"
  
