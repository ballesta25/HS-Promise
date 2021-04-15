{-# Language GADTs, KindSignatures #-}

import Control.Concurrent
import Control.Monad
import Data.Function


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
pThen' p k = runPromise k (return . reject) p

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
pCatch' p k = runPromise (return . resolve) k p

pJoin :: Promise f (Promise f p) -> IO (Promise f p)
pJoin pp = pThen' pp return


runPromise :: (p -> IO a) -> (f -> IO a) -> Promise f p -> IO a
runPromise yes no (Pending state) = do
  result <- readMVar state
  case result of
    Left x -> no x
    Right x -> yes x
runPromise yes _ (Fulfilled x) = yes x
runPromise _ no (Rejected x) = no x
runPromise yes no (PromiseMap g pr) = do
  pr' <- bimapPromise pr id g
  runPromise yes no pr'
runPromise yes no (PromiseMap2 g prA prB) = do
  pr' <- pThen' prA $ \a ->
    pThen' prB $ \b -> return $ resolve $ g a b
  runPromise yes no pr'
runPromise yes no (PromiseJoin pp) = do
  p <- pJoin pp
  runPromise yes no p
runPromise yes no (PromiseInvert pr) = runPromise no yes pr


waitBoth :: Promise f p -> Promise f p' -> IO (Promise f (p, p'))
waitBoth prA prB = runPromise
  (\a -> runPromise (\b -> pure $ resolve (a,b)) (pure . reject) prB)
  (pure . reject) prA

waitAll :: [Promise f p] -> IO (Promise f [p])
waitAll ps = undefined -- foldM _ _  ps --(\x -> pThen' undefined) undefined ps

waitOne :: Promise f p -> Promise f' p -> IO (Promise (f, f') p)
waitOne prA prB = fmap PromiseInvert (waitBoth (PromiseInvert prA) (PromiseInvert prB))  --fmap PromiseInvert . (waitBoth `on` PromiseInvert)

waitAny :: [Promise f p] -> Promise [f] p
waitAny ps = undefined



main = do
  promise <- newPromise $ \s f -> do
    threadDelay (2 * 1000 * 1000)
    s "promised."
  pThen promise $ \p -> putStrLn p
  promise2 <- pThen promise $ \p -> putStrLn "again?"
  pThen (PromiseMap (\() -> 1234)promise2) $ \p -> print p

testWait = (do
               p1 <- newPromise $ \s f -> do
                 threadDelay (3 * 1000 * 1000)
                 s "finished"
               p2 <- newPromise $ \s f -> s "pr 2"
               waitOne p1 p2)
           >>= runPromise putStrLn (putStrLn . const "both failed")
  

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
