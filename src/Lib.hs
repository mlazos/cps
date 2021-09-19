module Lib
    (
    ) where


import Control.Monad.Trans.Cont
import Control.Monad

-- Helper to transform value into suspended computation
-- Equivalent to return in the cont monad.
constCps :: a -> (a -> r) -> r
constCps x k = k x

-- add in CPS form
addCps :: Int -> Int -> (Int -> r) -> r
addCps x y k = constCps x $ \l -> constCps y $ \r -> k (l + r)

-- CallCC implementation
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h

-- Erroneous CallCC implementation which does not break out of control flow
callCC2 :: ((a -> Cont r a) -> Cont r a) -> Cont r a
callCC2 f = cont $ \h -> runCont (f (\a -> cont $ \k -> k a)) h

-- Broken example where k does not have a properly defined type
quux = Lib.callCC $ \k -> do
    let n = 5
    when True $ k n
    return 25 -- k 25
