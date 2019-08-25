import Helper (boxes , withoutzero , countelem , removerepetives , gridnumbers , getnumbers , replace2d , next , getitem , getfirst)

--------------------------------------------------------------------------------------------

checkrows :: [[Int]] -> Bool
checkrows [] = True
checkrows (x:xs) 
	| (length (withoutzero x)) == (length (removerepetives (withoutzero x))) = checkrows xs
	| otherwise = False


checkcols :: [[Int]] -> Int -> Bool
checkcols rows i
	| i == 9 = True
	| (length (withoutzero (getcol rows i))) == (length (removerepetives (withoutzero (getcol rows i)))) = checkcols rows (i+1) 
	| otherwise = False
	where
		getcol :: [[Int]] -> Int -> [Int]
		getcol [] _ = []
		getcol (row:xs) c = (row !! c):(getcol xs c)
		

checkgrids :: [[Int]] -> Int -> Bool
checkgrids rows i
	| i == 9 = True
	| (length (withoutzero (getgrid rows i))) == (length (removerepetives (withoutzero (getgrid rows i)))) = checkgrids rows (i+1) 
	| otherwise = False
	where
		getgrid :: [[Int]] -> Int -> [Int]
		getgrid rows i = [(rows !! r) !! c | (r,c) <- (boxes !! i)]

	
checkinput :: [Int] -> Bool
checkinput [] = False
checkinput xs 
	| length xs == 81 = (checknumbers True xs) && (checkrows placednumbers) && (checkcols placednumbers 0) && (checkgrids placednumbers 0)
	| otherwise = False
	where
		checknumbers :: Bool -> [Int] -> Bool
		checknumbers res [] = res 
		checknumbers res (x:xs)
			| x `elem` [1..9] || x == 0 = checknumbers (True && res) xs
			| otherwise = checknumbers (False && res) xs
		placednumbers :: [[Int]]
		placednumbers = gridnumbers xs

--------------------------------------------------------------------------------------------


solve :: [[Int]] -> (Int,Int) -> [(Int,Int)] -> [[Int]]
solve rs (9,9) _ = rs 
solve rs (r,c) pst 
	| (getitem (r,c) rs) == 0 =
		let (rs2 , (r2,c2) , pst2) = try rs (r,c) pst 1 
		in solve rs2 (next (r2,c2)) pst2
	| otherwise = solve rs (next (r,c)) pst
	where
		try :: [[Int]] -> (Int,Int) -> [(Int,Int)] -> Int -> ([[Int]] , (Int,Int) , [(Int,Int)])
		try rs (r,c) pst 10
			| pst == [] = error "wrong puzzels givens"
			| otherwise = try (replace2d (r,c) 0 rs) (getfirst pst) (tail pst) ((getitem (getfirst pst) rs) + 1) 
	
		try rs (r,c) pst st
			| st `elem` (getnumbers (r,c) rs) = try rs (r,c) pst (st+1) 
			| otherwise = ((replace2d (r,c) st rs) , (r,c) , ((r,c):pst))



calculate nums 
	| checkinput nums = solve (gridnumbers nums) (0,0) []
	| otherwise = error "numbers are not valid"














