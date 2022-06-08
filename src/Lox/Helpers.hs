module Lox.Helpers where 


unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = 
  unsnoc' [] xs 
  where 
    unsnoc' :: [a] -> [a] -> Maybe ([a], a)
    unsnoc' ys [x] = 
      Just (reverse ys, x)
    unsnoc' ys (x:xs) = 
      unsnoc' (x:ys) xs
    unsnoc' _ [] = Nothing
snoc :: [a] -> a -> [a]
snoc a b = a ++ pure b

ifM b t f = do b <- b; if b then t else f

infixr 2 <||>
infixr 3 <&&>
(<||>) a = ifM a (pure True)
(<&&>) t f = ifM t f (pure False)

anyM p = foldr ((<||>) . p) (pure False)
andM p = foldr ((<&&>) . p) (pure True)
