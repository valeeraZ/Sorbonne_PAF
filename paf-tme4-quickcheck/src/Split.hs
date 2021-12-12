module Split where

split :: Char -> String -> [String]
split delimiter s = case break (== delimiter) s of
    (x, y:ys) -> x : split delimiter ys
    (x, "") -> [x]

-- >>> split '/' "aa/bb/ccc/dd d"
-- ["aa","bb","ccc","dd d"]

-- >>> split ';' ";"
-- ["",""]

unsplit :: Char -> [String] -> String
unsplit delimiter [] = []
unsplit delimiter (x:xs) 
        | length xs > 1 = x ++ [delimiter] ++ (unsplit delimiter xs) 
        | not (null xs) = x ++ [delimiter] <> head xs
        | otherwise     = x

-- >>> unsplit '/' ["aa","bb","ccc","dd d"]
-- "aa/bb/ccc/dd d"

prop_split_unsplit :: Char -> String -> Bool
prop_split_unsplit c str = unsplit c (split c str) == str

-- >>> prop_split_unsplit 'a' "a"
-- True