



data Op a = Null | Rust a


-- where f is of kind * -> *. Could be [], Optional, or Set, ....
fmap :: (a -> b) -> f a -> f b




-- where m is of kind * -> *. Could be [], Optional
bind :: m a -> (a -> m b) -> m b 


listComp = [(a,b) | a <- [1,2,3], b <- [3,4,5,6], a+b == 4] -- [(1,3)]
listComp = do 
  a <- [1,2,3]
  b <- [4,5,6]
  if a + b == 4 then return (a,b) else return []

listComp = 
  [1,2,3] `bind` (\a -> 
    [4,5,5] `bind` \(b ->
      -- if statement
      [a + b]
    )
  )



f :: a -> b
g :: b -> c


combined :: a -> c
