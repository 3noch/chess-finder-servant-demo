module ChessFinder.Common where

infixr 1 .=
(.=) :: a -> b -> (a, b)
(.=) = (,)

else' :: a -> a -> Bool -> a
else' x y bool = if bool then x else y

enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

infixl 4 <<$>>
(<<$>>) ::(Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
(<<$>>) = fmap fmap fmap
