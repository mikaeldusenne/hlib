module Tuple where


third (_,_,c) = c


applyToTuple f (a,b) = (f a, f b)
applyToFst f (a,b)   = (f a,   b)
applyToSnd f (a,b)   = (  a, f b)


uncurry3 f (a,b,c) = f a b c

-- reduceTuple f (a,b) = f a b

zipTuplesWith f (a,b) (a',b') = (f a a', f b b')
zipTuples = zipTuplesWith (,)

reverseTuple (a,b) = (b,a)

headNtail (x:xs) = (x,xs)
headNtail [] = error "no headNtail on an empty list!"

toTuple2 [a,b] = (a,b)
toTuple3 [a,b,c] = (a,b,c)

duplicate a = (a,a)
swap (a,b) = (b,a)

collapse (a,b) = a ++ b

tupleToList (a, b) = [a, b]
