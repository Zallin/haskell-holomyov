module Stream where

data Stream a = a :& Stream a
  deriving (Show, Eq, Ord)

nats ::  Int -> Stream Int
nats a = a :& nats (a + 1)

shead :: Stream a -> a
shead (a :& _) = a

stail :: Stream a -> Stream a
stail (_ :& t) = t

sn :: Int -> Stream a -> a
sn 0 (a :& _) = a
sn n (_ :& rest) = sn (n - 1) rest

stake :: Int -> Stream a -> [a]
stake 0 _ = []
stake n (f :& rest) = f : stake (n - 1) rest

smap :: (a -> b) -> Stream a -> Stream b
smap f (a :& rest) = f a :& smap f rest

sfilter :: (a -> Bool) -> Stream a -> Stream a
sfilter f (a :& rest) = if f a
                        then a :& sfilter f rest
                        else sfilter f rest

szip :: Stream a -> Stream b -> Stream (a, b)
szip (a :& r1) (b :& r2) = (a, b) :& szip r1 r2

szipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
szipWith f (a :& r1) (b :& r2) = f a b :& szipWith f r1 r2

siterate :: (a -> a) -> a -> Stream a
siterate f s = s :& siterate f (f s)
