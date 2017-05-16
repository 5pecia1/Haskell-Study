--����Ʈ�� ������ ���Ҹ� �����ϴ� myLast 
myLast :: [a] -> a
myLast = foldl1 (\_ x -> x) 

--����Ʈ�� k��° ���Ҹ� �����ϴ� �Լ� elementAt
elementAt ::[a] ->Int -> a
elementAt [] _ = error "Error"
elementAt (x:xs) 1 = x
elementAt (x:xs) n 
  | n > 0 = elementAt xs (n-1)
  | otherwise = error "Error"

--����Ʈ�� palindrome���� �ƴ����� �����ϴ��Լ��� �ۼ�
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error "Error"
isPalindrome (x:xs)
  | (length (x:xs)) == 1 = True
  | x == (myLast xs) = isPalindrome (init xs)
  | otherwise = False

--���ӵ� ����Ʈ ���Ҹ� �����ϴ� compress �Լ��� �ۼ�
compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:xs)
  | x /= head xs = x : compress xs
  | otherwise = compress xs

--����Ʈ�� run-length ���ڵ����� �����ϴ� �Լ� encode�� �ۼ��Ͻÿ�
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = (countList xs, head xs) : encode (dropList xs)

dropList :: Eq a => [a] -> [a]
dropList xs = drop (countList (xs)) xs

countList :: Eq a => [a] -> Int
countList [] = 0
countList (x:xs) = length (takeWhile (== x) (x:xs))
