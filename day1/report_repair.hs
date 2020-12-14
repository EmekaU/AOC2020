import System.IO


main = do
    
    s <- readFile "nums.txt"
    let content = lines s
    let removeEnd = init(content)
    --let removeHead = tail(removeEnd)
    let result = rr (convertToInt removeEnd)
    print result


convertToInt :: [String] -> [Integer]
convertToInt xs = [ xi | x<-xs, let xi = read x :: Integer]

rr :: [Integer] -> Integer
rr xs = head [x*y | x <- xs, let y = getY x xs, y /= 0]

getY :: Integer -> [Integer] -> Integer
getY x xs = foldl (+) 0[val | val <- xs, x + val == 2020]

