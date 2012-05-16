{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.Monad.Par.Scheds.Edi
 (
  Par,
  Future,
--  IVar,
  runPar,
 )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Par.Class
import Control.Monad.Trans
import Control.Parallel.Eden (Trans)
import Control.Parallel.Eden.Edi hiding (fork)

newtype Par a = Par { runPar :: IO a }
  deriving Monad

{-data IVar a = IVar { iVarChan :: ChanName' a
                   , iVarVal  :: a }-}

newtype Future a = Future { unFuture :: a }

instance ParFuture Future Par where
  spawn m = Par $ do (c,x) <- createC
                     spawnProcessAt 0 $ sendNF c =<< runPar m
                     return $ Future x
  spawn_ m = Par $ do (c,x) <- createC
                      spawnProcessAt 0 $ sendWith rseq c =<< runPar m
                      return $ Future x
  get = return . unFuture

{-
instance ParFuture IVar Par where
  spawn m = do ivar <- new
               fork $ put ivar =<< m
               return ivar
  spawn_ m = do ivar <- new
                fork $ put_ ivar =<< m
                return ivar
  get ivar = iVarVal ivar `seq` return (iVarVal ivar)

instance ParIVar IVar Par where
  fork m = Par $ do
             spawnProcessAt 0 $ runPar m
             return ()
  new = Par $ uncurry IVar `liftM` createC
  put_ ivar = Par . sendWith rseq (iVarChan ivar)
-}
