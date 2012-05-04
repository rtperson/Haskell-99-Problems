fizzBuzz :: [Int] -> [String]
fizzBuzz ns = map (\(n,s) -> if (s /= "") then s else (show n)) $ fizz' ns
  where 
    fizz' :: [Int] -> [(Int, String)]
    fizz' xs = zip xs (zipWith (++) 
                       (map (\n -> if (n `mod` 3) == 0 then "fizz" else "") xs) 
                       (map (\n -> if (n `mod` 5) == 0 then "buzz" else "") xs))


-- List comprehensions, anyone?
boomBang :: [Int] -> [String]
boomBang ns = 
    [ if x `mod` 15 == 0
        then "boombang" 
        else if x `mod` 3 == 0
                then "boom" 
                else if x `mod` 5 == 0
                    then "bang" 
                    else show x 
      | x <- ns]


main = do
    mapM_ putStrLn $ fizzBuzz [1..100]