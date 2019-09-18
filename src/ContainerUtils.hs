module ContainerUtils(
    mapTuple,
    nth,
    unwrap,
    lastN
) where

-- Applies a maping function to a 2-tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- Safely retrieves the nth element of an array
nth :: Int -> [a] -> Maybe a
nth n xs
    | (n >= 0) || (n < length xs) = Just $ xs !! n
    | otherwise = Nothing

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

unwrap :: Maybe a -> a
unwrap = maybe (error "Tried to unwrap empty Maybe") id
