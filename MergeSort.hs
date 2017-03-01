import Data.List
import System.IO 

--Merging Function
merge :: (Num a,Ord a) => [a]->[a]->[a]

merge [] [] = [] 
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) 
	| y<=x = (y:merge (x:xs) ys)
	| x<y = (x:merge xs (y:ys))
	| otherwise = []
	
--MergeSort function
mergeSort :: (Ord a , Num a)=>[a]->[a]

mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
	where
		firstHalf = take ((length xs)`div`2) xs
		secondHalf = drop ((length xs)`div`2) xs
		