import Vivid hiding (next)
import Vivid.OSC
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar


unTimestamp :: Timestamp -> Double
unTimestamp (Timestamp x) = x

-- | proves Timestamps are ordinary numbers, measured in seconds
testFomat = do
  x <- getTime
  print x
  wait 1
  y <- getTime
  print y
  print $ diffTimestamps y x

-- | Do something every second, very nearly on the second
-- (consistently 1 to 3 ms after the second)
loop = do
  let loop = do x <- unTimestamp <$> getTime
                print x
                wait $ fromIntegral (ceiling x) - x
                unTimestamp <$> getTime >>= print
                print ""
                loop
  x <- forkIO loop
  getChar
  killThread x

next :: RealFrac a => a -> a -> a -> a
next time0 period now =
  fromIntegral (ceiling $ (now - time0) / period ) * period + time0

-- | If you execute the following commands, with a good wait in between each:
-- (l,ch) <- period
-- ch 0.5
-- killThread l
-- you should see a print statement looped with an initial period of 1 Hz,
-- which changes to 2 Hz when you run "ch 0.5".
period = do
  time0 <- (+0.01) . unTimestamp <$> getTime >>= newMVar
    -- adding .01 makes it start in .01 seconds rather than 1 second
  period <- newMVar 1
  let loop = do
        now <- unTimestamp <$> getTime

        -- changes to time0 or period won't affect this cycle
        time0' <- readMVar time0
        period' <- readMVar period

        let nextCycle = next time0' period' now
        wait $ nextCycle - now
        print $ show now
        loop
  let chPeriod :: Double -> IO ()
      chPeriod newPeriod = do
        now <- unTimestamp <$> getTime
        time0_value <- readMVar time0
        period_value <- readMVar period
        swapMVar time0 $ next time0_value period_value now
        swapMVar period newPeriod >> return ()
  l <- forkIO loop
  return (l,chPeriod)
