import Control.Monad
import Testi

--data D = D Int Int
data String = Int

print :: (Show b) => b -> Prelude.String
print x = show x

l [] = 0
l (x:xs) = 3 + l xs

-- | test (D x y) = x + y
tutu :: Int
tutu=4+a

hallo = tutu

halli = tutu

--bibi = tata

-- | the coolest function eva eva eva
main = Prelude.print $ test "x"
  where test :: Prelude.String -> Int
        test str = l str
