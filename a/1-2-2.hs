
import Data.Tuple.HT (thd3)

resetPlus :: (Int, Int, Int) -> Char -> (Int, Int, Int)
resetPlus (l, r, m) c
    | nr >  nl   = (0, 0, m)
    | nr == nl   = (nl, nr, (max m (2 * nr)))
    | otherwise  = (nl, nr, m)
        where nl = if c == '{' then l + 1 else l
              nr = if c == '}' then r + 1 else r

reverseSwap :: String -> String
reverseSwap = reverse . map (\ c -> if c == '{' then '}' else '{')

longestValidBraces :: String -> Int
longestValidBraces input = 
    max (thd3 $ foldl resetPlus (0, 0, 0) input)
        (thd3 $ foldl resetPlus (0, 0, 0) $ reverseSwap input)

main :: IO ()
main = do
    print $ longestValidBraces "{}"
    print $ longestValidBraces "{{}}"
    print $ longestValidBraces "{{}{}}{}"
    print $ longestValidBraces "}{}"
    print $ longestValidBraces "{{}"
