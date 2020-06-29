testAllWaiting = TestCase $ do
  now <- unTimestamp <$> getTime
  dist <- newDispatch
  let mm = mTimeMuseqs dist
  swapMVar mm $ M.fromList [("a",(now+1,emptyMuseq))
                           ,("b",(now+1,emptyMuseq))]
  assertBool "all waiting" =<< allWaiting dist
  swapMVar mm $ M.fromList [("a",(now-1,emptyMuseq))
                           ,("b",(now+1,emptyMuseq))]
  assertBool "one is not waiting" =<< not <$> allWaiting dist

testFindNextEvents = TestCase $ do
  let bp f = New Boop $ show f
      events = [(0,bp 1),(0.25,bp 2),(0.5,bp 3),(0.5,bp 4),(0.75,bp 5)]
  assertBool "testFindNextEvents, midway" $
    (findNextEvents 1 10 29 $ museq 2 events)
      -- findNextEvents time0 tempoPeriod now museq
    == (2, Prelude.map snd $ V.toList $ V.slice 2 2 $ V.fromList events)
      -- last phase 0 was at 21s, so now (29) is 2s before halfway through
  assertBool "testFindNextEvents, end (wraparound)" $
    (findNextEvents 1 10 39.5 $ museq 2 events)
    == (1.5, Prelude.map snd $ V.toList $ V.slice 0 1 $ V.fromList events)

  let events' = tail events -- PITFALL, maybe confusing: below I use both
  assertBool "testFindNextEvents, no zero event, this cycle" $
    (findNextEvents 1 50 123 $ museq 2 events') -- next is 126
    == (3, Prelude.map snd $ V.toList $ V.slice 1 1 $ V.fromList events)
  assertBool "testFindNextEvents, no zero event, next cycle" $
    (findNextEvents 1 50 100 $ museq 2 events') -- next is 126
    == (26, Prelude.map snd $ V.toList $ V.slice 1 1 $ V.fromList events)
