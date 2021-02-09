{-# Language GADTs, KindSignatures #-}

import Control.Concurrent


data Promise :: * -> * -> * where
  Pending :: MVar (Either f p) -> Promise f p
  Fulfilled :: p -> Promise f p
  Rejected :: f -> Promise f p


--newPromise :: ((SuccessFun) -> (FailFun) -> IO ()) -> Promise f p
newPromise :: ((p -> IO ()) -> (f -> IO ()) -> IO ()) -> IO (Promise f p)
newPromise k = do
  state <- newEmptyMVar
  forkIO $ k (putMVar state . Right) (putMVar state . Left)
  return (Pending state)

resolve :: p -> Promise f p
resolve x = Fulfilled x

reject :: f -> Promise f p
reject x = Rejected x



pThen :: Promise f p -> (p -> IO p') -> IO (Promise f p')
pThen pr@(Pending state) k = do
  result <- readMVar state
  case result of
    Left x -> return $ reject x
    Right x -> resolve <$> k x
pThen (Fulfilled x) k = resolve <$> k x
pThen (Rejected x)  k = return $ reject x


pCatch :: Promise f p -> (f -> IO f') -> IO (Promise f' p)
pCatch (Pending state) k = do
  result <- readMVar state
  case result of
    Left x -> reject <$> k x
    Right x -> return $ resolve x
pCatch (Fulfilled x) k = return $ resolve x
pCatch (Rejected x)  k = reject <$> k x


main = do
  promise <- newPromise $ \s f -> do
    threadDelay (2 * 1000 * 1000)
    s "promised."
  pThen promise $ \p -> putStrLn p
  promise2 <- pThen promise $ \p -> putStrLn "again?"
  pThen promise2 $ \p -> print p
  
-- fmap, ap, bind for (Promise f)
