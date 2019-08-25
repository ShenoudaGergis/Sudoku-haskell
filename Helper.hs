module Helper where

boxes :: [[(Int,Int)]]
boxes = [[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)],
		[(3,0),(3,1),(3,2),(4,0),(4,1),(4,2),(5,0),(5,1),(5,2)],
		[(6,0),(6,1),(6,2),(7,0),(7,1),(7,2),(8,0),(8,1),(8,2)],
		[(0,3),(0,4),(0,5),(1,3),(1,4),(1,5),(2,3),(2,4),(2,5)],
		[(3,3),(3,4),(3,5),(4,3),(4,4),(4,5),(5,3),(5,4),(5,5)],
		[(6,3),(6,4),(6,5),(7,3),(7,4),(7,5),(8,3),(8,4),(8,5)],
		[(0,6),(0,7),(0,8),(1,6),(1,7),(1,8),(2,6),(2,7),(2,8)],
		[(3,6),(3,7),(3,8),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8)],
		[(6,6),(6,7),(6,8),(7,6),(7,7),(7,8),(8,6),(8,7),(8,8)]];


withoutzero :: [Int] -> [Int]
withoutzero x = (filter (/= 0) x)

countelem :: Int -> [Int] -> Int
countelem _ [] = 0
countelem c (x:xs)
	| c == x = 1 + countelem c xs
	| otherwise = 0 + countelem c xs		

removerepetives :: [Int] -> [Int]
removerepetives xs = [c | c <- xs,(countelem c xs) == 1]


gridnumbers (x:xs) = grid 0 [x] xs
	where 
		grid :: Int -> [Int] -> [Int] -> [[Int]]
		grid _ res [] = [res]
		grid col res (x:xs)
			| col == 8 = [res] ++ (grid 0 [x] xs) 
			| otherwise = grid (col+1) (res++[x]) xs 
			
			
replace2d :: (Int,Int) -> Int -> [[Int]] -> [[Int]]
replace2d _ _ [] = []
replace2d (r,c) v (ro:rs)
	| r == 0 = [(replacer c v ro)] ++ (replace2d ((r-1),c) v rs) 
	| otherwise = [ro] ++ (replace2d ((r-1),c) v rs) 
	where 
		replacer :: Int -> Int -> [Int] -> [Int]
		replacer _ _ [] = []
		replacer c v (x:xs)
			| c == 0 = [v] ++ (replacer (c-1) v xs)
			| otherwise = [x] ++ (replacer (c-1) v xs)


getnumbers :: (Int,Int) -> [[Int]] -> [Int]
getnumbers (r,c) rows = withoutzero ((getcol rows c) ++ (rows !! r) ++ [(rows !! x) !! y | (x,y) <- (getbox (r,c) 0)])
	where
		getcol :: [[Int]] -> Int -> [Int]
		getcol [] _ = []
		getcol (row:xs) c = (row !! c):(getcol xs c)

		getbox :: (Int,Int) -> Int -> [(Int,Int)]
		getbox (r,c) i
			| (r,c) `elem` (boxes !! i) = boxes !! i
			| otherwise = getbox (r,c) (i+1)


next :: (Int,Int) -> (Int,Int)
next (8,8) = (9,9)
next (r,c)
	| c == 8 && r /= 8 = ((r+1),0)
	| otherwise = (r,(c+1))

getitem :: (Int,Int) -> [[Int]] -> Int 
getitem (r,c) rs = (rs !! r) !! c

getfirst :: [(Int,Int)] -> (Int,Int) 
getfirst (x:xs) = x