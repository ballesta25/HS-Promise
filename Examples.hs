-- examples/test cases of usage of promises

import Control.Concurrent
import Promise


main = do
  promise <- newPromise $ \s f -> do
    threadDelay (2 * 1000 * 1000)
    s "promised."
  pThen promise $ \p -> return . resolve $ putStrLn p
  promise2 <- pThen promise $ \p -> return . resolve $ putStrLn "again?"
  pThen (PromiseMap (\_ -> 1234) promise2) $ \p -> return . resolve $ print p

testWait = (do
               p1 <- newPromise $ \s f -> do
                 threadDelay (3 * 1000 * 1000)
                 s "finished"
               p2 <- newPromise $ \s f -> s "pr 2"
               pAny2 p1 p2)
           >>= runPromise putStrLn (putStrLn . const "both failed")


testRace = runPromise print (print :: () -> IO ()) =<< (do { x<-newPromise (\s f -> (threadDelay $ 6 * 1000 * 1000) >> s ()) ; y<-newPromise (\s f -> (threadDelay $ 2 * 1000 * 1000) >> s ()) ; pRace [x, y]})
