{-# Language GADTs, KindSignatures #-}

import Control.Concurrent


data Promise :: * -> * -> * where
  Pending :: MVar (Either f p) -> Promise f p
  Fulfilled :: p -> Promise f p
  Rejected :: f -> Promise f p
  PromiseMap :: (a -> b) -> Promise f a -> Promise f b


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
pThen (PromiseMap g pr) k = pThen pr (k . g)


pCatch :: Promise f p -> (f -> IO f') -> IO (Promise f' p)
pCatch (Pending state) k = do
  result <- readMVar state
  case result of
    Left x -> reject <$> k x
    Right x -> return $ resolve x
pCatch (Fulfilled x) k = return $ resolve x
pCatch (Rejected x)  k = reject <$> k x
pCatch (PromiseMap g pr) k = fmap g <$> pCatch pr k

bimapPromise :: Promise f p -> (f -> f') -> (p -> p') -> IO (Promise f' p')
bimapPromise (Pending state) f g = do
  result <- readMVar state
  case result of
    Left x -> return $ reject (f x)
    Right x -> return $ resolve (g x)
bimapPromise (Fulfilled x) f g = return $ resolve (g x)
bimapPromise (Rejected x)  f g = return $ reject  (f x)
bimapPromise (PromiseMap h pr) f g = bimapPromise pr f (g . h)

pCatch' :: Promise f p
        -> (f -> IO (Promise f' p))
        -> IO (Promise f' p)
pCatch' (Pending state) k = do
  result <- readMVar state
  case result of
    Left x -> k x
    Right x -> return $ resolve x
pCatch' (Fulfilled x) k = return $ resolve x
pCatch' (Rejected x)  k = k x
pCatch' (PromiseMap g pr) k = do
  pr' <- bimapPromise pr id (Right . g)
  caught <- pCatch' pr' (\y -> do pr'' <- k y
                                  return $ PromiseMap Left pr'')
  bimapPromise caught id (\z -> case z of
                                  (Left a) -> a
                                  (Right a) -> a)
  
main = do
  promise <- newPromise $ \s f -> do
    threadDelay (2 * 1000 * 1000)
    s "promised."
  pThen promise $ \p -> putStrLn p
  promise2 <- pThen promise $ \p -> putStrLn "again?"
  pThen (PromiseMap (\() -> 1234)promise2) $ \p -> print p
  

-- fmap, ap, bind for (Promise f)
instance Functor (Promise f) where
  fmap f pr = PromiseMap f pr

pMap :: (a -> b) -> (Promise f a) -> IO (Promise f b)
pMap f pr = pThen pr (return . f)

pAp :: Promise f (a -> b) -> Promise f a -> IO (Promise f b)
pAp f pr = undefined
