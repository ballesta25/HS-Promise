{-# Language GADTs, KindSignatures #-}

import Control.Concurrent

-- may not need this - MVar (Either f p) seem sufficient, with empty ~ pending
data PState = Pending | Fulfilled | Rejected deriving (Eq)

data Promise :: * -> * -> * where
  MkPromise :: MVar (Either f p) -> Promise f p
  -- pending ^, Fulfilled, Rejected, then resolve, reject don't need to be in IO

--newPromise :: ((SuccessFun) -> (FailFun) -> IO ()) -> Promise f p
newPromise :: ((p -> IO ()) -> (f -> IO ()) -> IO ()) -> IO (Promise f p)
newPromise k = do
  state <- newEmptyMVar
  forkIO $ k (putMVar state . Right) (putMVar state . Left)
  return (MkPromise state)

resolve :: p -> IO (Promise f p)
resolve x = newPromise (\p _ -> p x)

reject :: f -> IO (Promise f p)
reject x = newPromise (\_ f -> f x)


{-
pDone :: Promise f p -> IO ()
pDone (MkPromise state) = do
  result <- takeMVar state
  case result of
    Left _ -> undefined
-}

pThen :: Promise f p -> (p -> IO p') -> IO (Promise f p')
pThen pr@(MkPromise state) k = do
  result <- readMVar state
  case result of
    Left x -> reject x
    Right x -> k x >>= resolve
  

pCatch :: Promise f p -> (f -> IO ()) -> IO ()
pCatch (MkPromise state) k = do
  result <- readMVar state
  case result of
    Left x -> k x
    Right _ -> return ()


main = do
  promise <- newPromise $ \s f -> do
    threadDelay (2 * 1000 * 1000)
    s "promised."
  pThen promise $ \p -> putStrLn p
  promise2 <- pThen promise $ \p -> putStrLn "again?"
  pThen promise2 $ \p -> print p
  
