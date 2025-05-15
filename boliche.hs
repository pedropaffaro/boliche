main :: IO ()
main = do
    line <- getLine
    let plays = map (read :: String -> Int) (words line)
    putStrLn $ printFrames (frames plays) ++ show (score 0 (frames plays))

frames :: [Int] -> [[Int]]
frames = throw 1
    where
        throw _ [] = []
        throw 10 rolls = [take 3 rolls]
        throw n (10:xs) = [10] : throw (n + 1) xs
        throw n (f:s:xs) = [f, s] : throw (n + 1) xs

isStrike :: [Int] -> Bool
isStrike [10] = True
isStrike _ = False

isSpare :: [Int] -> Bool
isSpare [f, s] = f + s == 10
isSpare _ = False

strikeBonus :: [[Int]] -> Int
strikeBonus (f:fs)
      | length f >= 2 = sum (take 2 f)
      | length f == 1, not (null fs) = head f + head (head fs)
      | otherwise = sum f

spareBonus :: [[Int]] -> Int
spareBonus ((x:_):_) = x
spareBonus _ = 0

score :: Int -> [[Int]] -> Int
score s [] = s
score s (x:xs)
    | isStrike x = score (s + 10 + strikeBonus xs) xs
    | isSpare x  = score (s + 10 + spareBonus xs) xs
    | otherwise  = score (s + sum x) xs
    
printFrames :: [[Int]] -> String
printFrames fs = concatMap printFrame (zip [1..] fs)

printFrame :: (Int, [Int]) -> String
-- Frame normal com strike
printFrame (n, [10]) | n < 10 = "X _ | "
-- Frame normal com spare
printFrame (n, [x, y]) | x + y == 10 && x /= 10 && n < 10 = show x ++ " / | "
-- Frame normal sem strike/spare
printFrame (n, [x, y]) | n < 10 = show x ++ " " ++ show y ++ " | "
-- Décimo frame
printFrame (10, rolls) = unwords (formatTenth rolls) ++ " | "
-- Outros casos (só por segurança)
printFrame _ = ""

formatTenth :: [Int] -> [String]
formatTenth [] = []
formatTenth [x] = [showRoll x]
formatTenth [x,y] = [showRoll x, showTenthSecond x y]
formatTenth (x:y:z:_) = [showRoll x, showTenthSecond x y, showTenthThird y z]

-- Helpers para décimo frame
showTenthSecond :: Int -> Int -> String
showTenthSecond x y
    | x /= 10 && x + y == 10 = "/"
    | y == 10 = "X"
    | otherwise = showRoll y

showTenthThird :: Int -> Int -> String
showTenthThird y z
    | y /= 10 && y + z == 10 = "/"
    | z == 10 = "X"
    | otherwise = showRoll z

showRoll :: Int -> String
showRoll 10 = "X"
showRoll 0 = "0"
showRoll n = show n