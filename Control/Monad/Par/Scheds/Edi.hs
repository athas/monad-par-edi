{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies #-}

module Control.Monad.Par.Scheds.Edi
 (
  Par,
  IVar,
  runPar,
  ParChan(..),
  Send,
  Recv
 )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Par.Class
import Control.Monad.Trans
import Control.Parallel.Eden.Edi hiding (fork)
import Control.Parallel.Eden.ParPrim hiding (fork)

newtype Par a = Par { runPar :: IO a }
  deriving Monad

data IVar a = IVar { iVarReadChan :: ChanName' [ChanName' a]
                   , iVarWriteChan :: ChanName' a
                   }

killIVar :: IVar a -> Par ()
killIVar ivar = Par $ do connectToPort (iVarReadChan ivar)
                         sendData Data []

instance ParFuture IVar Par where
  spawn m = do ivar <- new
               fork $ put ivar =<< m
               return ivar
  spawn_ m = do ivar <- new
                fork $ put_ ivar =<< m
                return ivar
  get ivar = Par $ do (c,x) <- createC
                      connectToPort (iVarReadChan ivar)
                      sendData Stream $ Right c
                      return x

instance ParIVar IVar Par where
  fork m = Par $ do
             spawnProcessAt 0 $ runPar m
             return ()
  new = Par $ do
          (rdc,rd) <- createC
          (wrc,wr) <- createC
          k <- selfPe
          forkIO $ do let val = wr
                          cs  = rd
                      mapM_ (\c -> sendWith rseq c val) cs
          return $ IVar rdc wrc
  put_ ivar x = Par $ sendWith rseq (iVarWriteChan ivar) x

-- This class is from Control.Monad.Par.Class, but not exported:

-- | @ParChan@ provides communication via streams of values between
--   computations in a Par monad.  Channels in this case are split
--   into separate send and receive ports.
--
--   The critical thing to know about @Chan@s in @Par@ monads is that
--   while the @recv@ method destructively advances the position of
--   the consumer's \"cursor\" in the stream, this is only observable
--   in the /local/ @Par@ thread.  That is, at @fork@ points it is
--   necessary to give the child computation a separate set of stream
--   cursors so that it observes the same sequences as the parent.
class Monad m => ParChan snd rcv m | m -> snd, m -> rcv where
   -- | Create a new communication channel, with separate send and receive ports.
   newChan :: m (snd a, rcv a)
   -- | Receive a message on a channel in a synchronous, blocking manner.
   recv    :: rcv a -> m a
   -- | Send a message on a channel.  This may or may not block.
   send    :: snd a -> a -> m ()

newtype Send a = Send (ChanName' [a])
newtype Recv a = Recv (MVar [a])

instance ParChan Send Recv Par where
  newChan = Par $ do (c,x) <- createC
                     rcv <- newMVar x
                     return (Send c, Recv rcv)
  recv (Recv mvar) = Par $ modifyMVar mvar fetch
    where fetch []     = error "End of stream"
          fetch (x:xs) = return (xs,x)
  send (Send c) x = Par $ do
                      connectToPort c
                      sendData Stream x
