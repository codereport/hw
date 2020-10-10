
isValidBraces :: String -> Bool
isValidBraces input = (minValue == 0) && (lastValue == 0)
    where leftBraces  = map fromEnum $ map (=='{') input
          rightBraces = map (+(-1)) leftBraces
          braces      = zipWith (+) leftBraces rightBraces
          bracesScan  = scanl1 (+) braces
          minValue    = minimum bracesScan
          lastValue   = last bracesScan

main :: IO ()
main = do
    print $ isValidBraces "{}"
    print $ isValidBraces "{{}}"
    print $ isValidBraces "{{}{}}{}"
    print $ isValidBraces "}{}"
    print $ isValidBraces "{{}"
