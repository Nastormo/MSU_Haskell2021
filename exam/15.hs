import Control.Concurrent (threadDelay, MVar, newMVar, putMVar, tryTakeMVar, takeMVar)

import Data.Maybe

import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

main :: IO ()
main = do
    stop <- newMVar 0
    _ <- installHandler sigINT (Catch $ onSigInt stop) Nothing
    loop stop


loop :: MVar (Int) -> IO ()
loop stop = do
    stopRequested <- takeMVar stop
    putMVar stop stopRequested
    case stopRequested of
        2 -> do 
            putStrLn "EXIT!"
            return ()
        1 -> do
            threadDelay 2000000
            stopRequested1 <- takeMVar stop
            if stopRequested1 /= 2 
                then do 
                    putMVar stop 0
                else do
                    putMVar stop 2
            loop stop
        0 -> do 
            threadDelay 1000000
            loop stop

onSigInt :: MVar (Int) -> IO ()
onSigInt stop = do
    valStop <- takeMVar stop
    putStrLn "sigINT"
    putMVar stop (valStop + 1)
