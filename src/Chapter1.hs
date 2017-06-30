module Chapter1 where

-- given an element and a sorted list,
-- insert the element in the correct place
insert :: Ord a => a -> [a] -> [a]
insert this [] = [this] 
insert this (first:rest)
    | this <= first = this:first:rest
    | this >  first = first:insert this rest


-- a fold implimentation of insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []


-- a variant that take in an ordering
insertionSortOrder :: Ord a => Ordering -> [a] -> [a]
insertionSortOrder _ord = case _ord of
        LT -> reverse . insertionSort
        _  -> insertionSort


-- find an element in a list if contained wrapped in a Maybe context
findInSequence :: Eq a => [a] -> a -> Maybe Int
findInSequence [] _ = Nothing
findInSequence (this:rest) target
    | target == this = Just 0
    | otherwise      = fmap (+1) (findInSequence rest target)


-- mergesort
mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list = let (_fst, _snd) = _mergeSplit list
                 in foldr insert (mergeSort _fst) (mergeSort _snd)

    where _mergeSplit this = let midway = length this `div` 2
                             in (take midway this, drop midway this)


-- horner's rule for polynomial evaluation
hornersRule :: Num a => [a] -> a -> a
hornersRule coeffs x = foldr (\ci cj -> ci + x*cj) 0 coeffs


-- generate invertions of a sequence
inversions :: Ord a => [a] -> [(Int, Int)]
inversions this = let indices = [0..(length this - 1)]
                  in [(i,j) | i <- indices
                            , j <- indices
                            , i < j
                            , this !! i > this !! j]


-- the classical binary search
-- NB assumes the list is sorted
binarySearch :: Eq a => [a] -> a -> Maybe Int
binarySearch = findIt . splitList

    where splitList this = let midway = length this `div` 2
                           in (take midway this, drop midway this, midway)

          findIt = undefined
