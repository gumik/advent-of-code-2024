module Something where
import Prelude hiding ((>>=))
-- import Prelude hiding ((>>=), return)

-- (>=>) :: (a -> m b) -> (b -> m b) -> (a -> m c)
-- (>=>) = undefined

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>=) = undefined

-- return :: a -> m a
-- return = undefined

-- f :: a -> Maybe a
-- f = undefined

-- g :: b -> Maybe b
-- g = undefined


-- h = f >=> g


-- ffmap :: Monad m => (a -> b) -> m a -> m b
-- ffmap f ma = ma >>= (return . f)


-- x = ffmap (+1) (Just 5)

(>>=) :: Either a b -> (b -> Either a c) -> Either a c
Left a  >>= f  =  Left a
Right b >>= f  =  f b

e1 = Left "error"  >>=  (\x -> Right (x + 1))
e2 = Right 5 >>= (\x -> Right (x + 1))
