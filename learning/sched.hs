import Vivid
import Vivid.OSC
import Control.Concurrent (forkIO, killThread)


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
