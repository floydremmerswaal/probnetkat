data Field = Field { name :: String, value :: Int } deriving (Show)
type Packet = [Field]
type History = [Packet]

data PPacket = PPacket { packet :: Packet, prob :: Float } deriving (Show)

-- assign value to the head package of a history
pAssH :: History -> Field -> History
pAssH [] _ = []
pAssH (x:xs) field = pAss x field:xs

-- assign value to a specific field in a packet
pAss :: Packet -> Field -> Packet
pAss [] _ = []
pAss (x:xs) f = if name x == name f then f:xs else x:pAss xs f

pDupp :: History -> History
pDupp [] = []
pDupp (x:xs) = [x,x] ++ xs

printHead :: History -> IO ()
printHead [] = print "Empty history"
printHead (x:xs) = do
    print x

-- Path: main.hs
main :: IO ()
main = do
    let history = [ [Field "a" 1, Field "b" 2, Field "c" 3]
                  , [Field "a" 4, Field "b" 5, Field "c" 6]
                  , [Field "a" 7, Field "b" 8, Field "c" 9]
                  ]
    printHead []
    printHead history
    let history' = pDupp history
    let history'' = pAssH history' (Field "a" 10)
    printHead history''
    let history''' = pAssH history'' (Field "b" 11)
    printHead history'''
    let history'''' = pAssH history''' (Field "c" 12)
    printHead history''''