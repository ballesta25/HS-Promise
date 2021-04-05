{-# Language GADTs, KindSignatures #-}

import Control.Concurrent
import Control.Monad


data Promise :: * -> * -> * where
  Pending :: MVar (Either f p) -> Promise f p
  Fulfilled :: p -> Promise f p
  Rejected :: f -> Promise f p
  PromiseMap :: (a -> b) -> Promise f a -> Promise f b
  PromiseMap2 ::  (a -> b -> c) -> (Promise f a) -> (Promise f b) -> (Promise f c)
  PromiseJoin :: (Promise f (Promise f a)) -> Promise f a
  PromiseInvert :: (Promise p f) -> Promise f p

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


pThen' :: Promise f p
        -> (p -> IO (Promise f p'))
        -> IO (Promise f p')
pThen' pr@(Pending state) k = do
  result <- readMVar state
  case result of
    Left x -> pThen' (Rejected x) k
    Right x -> pThen' (Fulfilled x) k
pThen' (Fulfilled x) k = k x
pThen' (Rejected x)  k = return $ reject x
pThen' (PromiseMap g pr) k = pThen' pr (k . g)
pThen' (PromiseMap2 g prA prB) k =
  pThen' prA $ \a ->
  pThen' prB $ \b ->
                 k $ g a b
pThen' (PromiseJoin pp) k = 
  pThen' pp $ \p ->
  pThen' p k
pThen' (PromiseInvert p) k = PromiseInvert <$> pCatch' p _

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
  pr' <- bimapPromise pr id g
  pCatch' pr' k
pCatch' (PromiseMap2 g prA prB) k = do
  pr' <- pThen' prA (\a ->
    pThen' prB $ \b -> return $ resolve (g a b))
  pCatch' pr' k
pCatch' (PromiseJoin pp) k = do
  p <- pJoin pp
  pCatch' p k

pJoin :: Promise f (Promise f p) -> IO (Promise f p)
pJoin pp = pThen' pp return

-- invertPromise :: Promise f p -> Promise p f
-- invertPromise (Fulfilled x) = Rejected x
-- invertPromise (Rejected x) = Fulfilled x
-- invertPromise (Pending state) = 

waitAny :: [Promise f p] -> Promise [f] p
waitAny ps = undefined

waitAll :: [Promise f p] -> Promise f [p]
waitAll ps =  undefined --foldM (\x -> pThen' undefined) undefined ps

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

instance Applicative (Promise f) where
  pure x = resolve x
  f <*> a = PromiseMap2 ($) f a

instance Monad (Promise f) where
  return = pure
  p >>= k = PromiseJoin (fmap k p)

pMap :: (a -> b) -> (Promise f a) -> IO (Promise f b)
pMap f pr = pThen pr (return . f)

pAp :: Promise f (a -> b) -> Promise f a -> IO (Promise f b)
pAp f pr = undefined

--pLift2 :: (a -> b -> c) -> (Promise f a) -> (Promise f b) -> (Promise f c)
--pLift2 g prA prB
