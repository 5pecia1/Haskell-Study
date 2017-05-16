--리스트의 마지막 원소를 리턴하는 myLast 
myLast :: [a] -> a
myLast = foldl1 (\_ x -> x) 

--리스트의 k번째 원소를 리턴하는 함수 elementAt
elementAt ::[a] ->Int -> a
elementAt [] _ = error "Error"
elementAt (x:xs) 1 = x
elementAt (x:xs) n 
  | n > 0 = elementAt xs (n-1)
  | otherwise = error "Error"

--리스트가 palindrome인지 아닌지를 리턴하는함수를 작성
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "Error"
isPalindrome (x:xs)
  | (length (x:xs)) == 1 = True
  | x == (myLast xs) = isPalindrome (init xs)
  | otherwise = False

--연속된 리스트 원소를 제거하는 compress 함수를 작성
compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:xs)
  | x /= head xs = x : compress xs
  | otherwise = compress xs

--리스트를 run-length 인코딩으로 리턴하는 함수 encode를 작성하시오
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = (countList xs, head xs) : encode (dropList xs)

dropList :: Eq a => [a] -> [a]
dropList xs = drop (countList (xs)) xs

countList :: Eq a => [a] -> Int
countList [] = 0
countList (x:xs) = length (takeWhile (== x) (x:xs))
