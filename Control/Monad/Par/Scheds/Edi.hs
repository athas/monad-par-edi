{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Control.Monad.Par.Scheds.Edi
 (
  Par,
  IVar,
  runPar,
 )
where

import Control.Monad
import Control.Monad.Par.Class
import Control.Monad.Trans
import Control.Parallel.Eden.Edi hiding (fork)

newtype Par a = Par { runPar :: IO a }
  deriving Monad

data IVar a = IVar { iVarChan :: ChanName' a
                   , iVarVal  :: a }

instance ParFuture IVar Par where
  spawn m = do ivar <- new
               fork $ put ivar =<< m
               return ivar
  spawn_ m = do ivar <- new
                fork $ put_ ivar =<< m
                return ivar
  get ivar = iVarVal ivar `seq` return (iVarVal ivar)

instance ParIVar IVar Par where
  fork = Par . spawnProcessAt 0 . runPar
  new = Par $ uncurry IVar `liftM` createC
  put_ ivar = Par . sendWith rseq (iVarChan ivar)
