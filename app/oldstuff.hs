-- -- some actual probabilistic stuff

-- -- distribution that has probability p for h1, and 1-p for h2
-- mProbH :: MonadDistribution m => Double -> History -> History -> m History
-- mProbH p h1 h2 = do
--   bernoulli p >>= \b -> return $ if b then h1 else h2

-- -- lift a history to a distribution over that history
-- mHistory :: MonadDistribution m => History -> m History
-- mHistory = return

-- -- mDrop returns the distribution over the empty history
-- mDrop :: MonadDistribution m => m History
-- mDrop = return []

-- -- mDropP maps any distribution to a distribution only containing the empty history
-- mDropP :: MonadDistribution m => m a -> m History
-- mDropP d = do
--   _ <- d
--   return []

-- -- mSkip does not change the distribution
-- mSkip = id

-- -- mITE is a probabilistic if-then-else, for tests
-- mITE :: MonadDistribution m => m Bool -> m a -> m a -> m a
-- mITE b d1 d2 = do
--   x <- b
--   if x then d1 else d2

-- -- test in probnetkat, takes in a history distribution, a field name and a value
-- mTest :: MonadDistribution m => m History -> String -> Int -> m History
-- mTest d name value = do
--   -- if the field 'name' of history 'd' has value 'value', return d, else return the empty history
--   d >>= \h -> if count (Field name value) (head h) > 0 then return h else mDrop


-- --------------



-- mixDist :: MonadDistribution m => Double -> m a -> m a -> m a
-- mixDist p d1 d2 = do
--   x <- bernoulli p
--   if x then d1 else d2

-- -- maps distribution over history to the same distribution but with the head of the history duplicated
-- mDupp :: MonadDistribution m => m History -> m History
-- mDupp d = do
--   h <- d
--   return $ [head h, head h] ++ tail h

-- -- test some functions with IO
-- testBench :: IO ()
-- testBench = do
--   print "Test bench starting"
--   let packet1 = [Field "pt" 1, Field "sw" 2]
--   let history1 = [packet1]

--   let nsamples = 1

--   -- lift the histories to distribtuions
--   let d1 = mHistory history1

--   -- duplicate the head of history1
--   let d1' = mDupp d1

--   -- sample from d1'
--   samples <- sampleIOfixed $ replicateM nsamples d1'

--   -- print samples in d1'
--   print "Samples in d1"
--   print samples

--   print "Test Done"

-- -- more complex testing
-- testBench2 :: IO ()
-- testBench2 = do
--   -- we will create a package (history) and then transform it, and finally sampling it to see the possibilities

--   print "Test bench 2 starting"
--   let packet1 = [Field "pt" 1, Field "sw" 2]

--   let history1 = [packet1]

--   let nsamples = 5

--   -- lift the histories to distribtuions
--   let d1 = mHistory history1

--   -- duplicate the head of history1
--   let d1' = mDupp d1

--   let d1'' = mixDist 0.5 d1' mDrop

--   -- sample from d1'
--   samples <- sampleIOfixed $ replicateM nsamples d1''

--   -- print samples in d1'
--   print "Samples in d1"
--   print samples

--   print "Test bench 2 done"


-- testTest :: IO ()
-- testTest = do
--   let packet1 = [Field "pt" 1, Field "sw" 2]
--   let packet2 = [Field "pt" 2, Field "sw" 2]
--   let d1 = mHistory [packet1]
--   let d2 = mHistory [packet2]

--   let dist = mixDist 0.5 d1 d2

--   let nsamples = 3
  
--   samples <- sampleIOfixed $ replicateM nsamples dist

--   let dist' = mTest dist "pt" 1

--   samples' <- sampleIOfixed $ replicateM nsamples dist'

--   print samples

--   print " "

--   print samples'


-- -- oldTest :: IO ()
-- oldTest = do
--   let f1 = Field "a" 1
--   let f2 = Field "a" 2

--   let p1 = [f1]
--   let p2 = [f2]

--   let h1 = [p1]
--   let h2 = [p2]

--   let nsamples = 1000

--   -- sampleIO vs sampleIOfixed (fixed seed or not)

--   samples' <- sampleIO $ replicateM nsamples (mHistory h1)

--   print $ count h1 samples'

--   samples'' <- sampleIO $ replicateM nsamples (mixDist 0.5 (mHistory h1) (mHistory h2))

--   print $ count h1 samples''
--   print $ count h2 samples''

--   testBench2
  
  -- count the number of 1's in samples
  -- print $ foldl (\acc x -> if x == p1 then acc + 1 else acc) 0 samples

  -- -- count the number of 2's in samples
  -- print $ foldl (\acc x -> if x == p2 then acc + 1 else acc) 0 samples


-- -- not really useful probably but just for testing

-- pDupp :: History -> History
-- pDupp [] = []
-- pDupp (x:xs) = [x,x] ++ xs

-- printHead :: History -> IO ()
-- printHead [] = print "Empty history"
-- printHead (x:xs) = print x

-- testRegularFunctions = do
--     let history = [ [Field "a" 1, Field "b" 2, Field "c" 3]
--                   , [Field "a" 4, Field "b" 5, Field "c" 6]
--                   , [Field "a" 7, Field "b" 8, Field "c" 9]
--                   ]
--     printHead []
--     printHead history
--     let history' = pDupp history
--     let history'' = assignHead history' (Field "a" 10)
--     printHead history''
--     let history''' = assignHead history'' (Field "b" 11)
--     printHead history'''
--     let history'''' = assignHead history''' (Field "c" 12)
--     printHead history''''
