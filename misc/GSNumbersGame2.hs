-- For GS Code Golf - Numbers Game
-- Read Doubles from input, output min, max, average and variance
-- 145 characters (excluding import)
import Data.List
main=do
i<-getLine
let r=sort$words i;[c,d,e]=foldl f[0,0,0]r;f v s=let n=read s in zipWith(+)v[n,n*n,1]
print(r!!0,last r,c,c/e,(d-c*c/e)/(e-1))