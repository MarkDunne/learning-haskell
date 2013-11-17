deerInZooDivTwo :: (RealFrac a, Integral b) => a -> a -> (b, b)
deerInZooDivTwo n k = (x, y)
    where x = round(n - k)
          y = floor(n - k / 2)
          
main = print $ test 3