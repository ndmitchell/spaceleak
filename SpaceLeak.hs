
module SpaceLeak(module SpaceLeak, force, measureStack, NFData(..)) where

import Control.DeepSeq
import Control.Exception
import Control.Concurrent.Extra
import System.IO.Unsafe
import Data.IORef
import Control.Monad
import Data.List


{-# NOINLINE wrapper1 #-}
wrapper1 :: a -> a
wrapper1 x = x

{-# NOINLINE wrapper2 #-}
wrapper2 :: a -> a
wrapper2 x = x

{-# NOINLINE wrapper3 #-}
wrapper3 :: a -> a
wrapper3 x = x

deepSeq :: NFData a => a -> b -> b
deepSeq a b = rnf a `seq` b


newThread :: a -> a
newThread a = unsafePerformIO $ join $ onceFork $ return $! a

newThread'' :: NFData a => a -> a
newThread'' = newThread . force

foldr' f z xs = foldl' (flip f) z $ reverse xs
foldr'' f z xs = foldl'' (flip f) z $ reverse xs

foldl'' f = foldl' (\a b -> force $ f a b)


measureStack :: IO Int
measureStack = do
    ref <- newIORef 0
    res <- try $ evaluate $ foldr (+) 0 $ map (\x -> unsafePerformIO $ do writeIORef ref x; return x) [1..1000000]
    case res of
        Left StackOverflow -> readIORef  ref
        Right v -> error "Stack did not overflow, so can't measure stack"
