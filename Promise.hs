{-# Language GADTs, KindSignatures #-}

module Promise where

import Control.Concurrent
import Control.Monad
import Data.Function
import Data.Void

data Promise :: * -> * -> * where
  Pending :: MVar (Either f p) -> Promise f p
  Fulfilled :: p -> Promise f p
  Rejected :: f -> Promise f p
  PromiseMap :: (a -> b) -> Promise f a -> Promise f b
  PromiseMap2 ::  (a -> b -> c) -> (Promise f a) -> (Promise f b) -> (Promise f c)
  PromiseJoin :: (Promise f (Promise f a)) -> Promise f a
  PromiseInvert :: (Promise p f) -> Promise f p

--newPromise :: ((SuccessFun) -> (FailFun) -> IO ()) -> Promise f p
newPromise :: ((p -> IO Token) -> (f -> IO Token) -> IO Token) -> IO (Promise f p)
newPromise k = do
  state <- newEmptyMVar
  forkIO $ fmap (const ()) $ k (fmap (const MkToken) . putMVar state . Right) (fmap (const MkToken) . putMVar state . Left)
  return (Pending state)

-- do not export the constructor; outside this module you only get one by calling a promise callback
data Token = MkToken

resolve :: p -> Promise f p
resolve x = Fulfilled x

reject :: f -> Promise f p
reject x = Rejected x

pThen :: Promise f p
        -> (p -> IO (Promise f p'))
        -> IO (Promise f p')
pThen p k = runPromise k (return . reject) p


bimapPromise :: Promise f p -> (f -> f') -> (p -> p') -> IO (Promise f' p')
bimapPromise pr f g = runPromise (return . resolve . g) (return . reject . f) pr

pCatch :: Promise f p
        -> (f -> IO (Promise f' p))
        -> IO (Promise f' p)
pCatch p k = runPromise (return . resolve) k p

pFinally :: Promise f p
         -> IO (Promise f' p')
         -> IO (Promise f' p')
pFinally p k = runPromise (const k) (const k) p

pJoin :: Promise f (Promise f p) -> IO (Promise f p)
pJoin pp = pThen pp return


runPromise :: (p -> IO a) -> (f -> IO a) -> Promise f p -> IO a
runPromise yes no (Pending state) = do
  result <- readMVar state
  case result of
    Left x -> no x
    Right x -> yes x
runPromise yes _ (Fulfilled x) = yes x
runPromise _ no (Rejected x) = no x
runPromise yes no (PromiseMap g pr) = runPromise (yes . g) no pr
runPromise yes no (PromiseMap2 g prA prB) = do
  pr' <- pThen prA $ \a ->
    pThen prB $ \b -> return $ resolve $ g a b
  runPromise yes no pr'
runPromise yes no (PromiseJoin pp) = do
  p <- pJoin pp
  runPromise yes no p
runPromise yes no (PromiseInvert pr) = runPromise no yes pr

await :: Promise f p -> IO  (Either f p)
await = runPromise (return . Right) (return . Left)


pAll2 :: Promise f p -> Promise f p' -> IO (Promise f (p, p'))
pAll2 prA prB = fmap PromiseInvert (pAny2 (PromiseInvert prA) (PromiseInvert prB))


pAll :: [Promise f p] -> IO (Promise f [p])
pAll [] = return $ resolve []
pAll (x:xs) = do
  prs <- pAll xs
  pr <- pAll2 x prs
  pThen pr (return . resolve . uncurry (:))


pAny2 :: Promise f p -> Promise f' p -> IO (Promise (f, f') p)
-- impl adapted from http://conal.net/blog/posts/functional-concurrency-with-unambiguous-choice
pAny2 prA prB = do v <- newEmptyMVar
                   errA <- newEmptyMVar
                   ta <- forkIO $ runPromise (putMVar v . Right) (putMVar errA) prA
                   tb <- forkIO $ runPromise (putMVar v . Right)
                         (\b -> do a <- takeMVar errA
                                   putMVar v $ Left (a, b)) prB
                   x <- takeMVar v
                   killThread ta
                   killThread tb
                   return $ case x of
                              Left (a, b) -> reject (a, b)
                              Right p -> resolve p
--pAny2 prA prB = fmap PromiseInvert (pAll2 (PromiseInvert prA) (PromiseInvert prB))  --can't be ```fmap PromiseInvert . (pAll2 `on` PromiseInvert)``` because invert is polymorphic in `f` and `p`

pAny :: [Promise f p] -> IO (Promise [f] p)
pAny [] = return $ reject []
pAny (x:xs) = do
  prs <- pAny xs
  pr <- pAny2 x prs
  pCatch pr (return . reject . uncurry (:))
                          

pAllSettled :: [Promise f p] -> IO (Promise f' [Either f p])
pAllSettled [] = return $ resolve []
pAllSettled (x:xs) = do v <- newEmptyMVar
                        forkIO $ await x >>= putMVar v
                        prs <- pAllSettled xs
                        a <- takeMVar v
                        pThen prs $ return . resolve . (a:)


pRace2 :: Promise f p -> Promise f p -> IO (Promise f p)
pRace2 prA prB = do v <- newEmptyMVar
                    ta <- forkIO $ await prA >>= putMVar v
                    tb <- forkIO $ await prB >>= putMVar v
                    x <- takeMVar v
                    killThread ta
                    killThread tb
                    return $ case x of
                               Left f -> reject f
                               Right p -> resolve p

pRace :: [Promise f p] -> IO (Promise f p)
pRace [] = newPromise (\s f -> return MkToken) -- remain Pending forever by never calling either the success handler or the failure handler
pRace (x:xs) = do
  prs <- pRace xs
  pRace2 x prs


  

instance Functor (Promise f) where
  fmap f pr = PromiseMap f pr

instance Applicative (Promise f) where
  pure x = resolve x
  f <*> a = PromiseMap2 ($) f a

instance Monad (Promise f) where
  return = pure
  p >>= k = PromiseJoin (fmap k p)
