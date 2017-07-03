module Spec (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Lib as C1
import qualified Data.List as List


-- All sorts specialised to ints
sortSpec :: ([Integer] -> [Integer]) -> [Integer] -> Bool
sortSpec f list = 
    List.sort list == f list


-- a special case for the insertion sort variant
sortSpecOrdering :: [Integer] -> Bool
sortSpecOrdering list =
    List.sortBy (flip compare) list ==
    C1.insertionSortOrder LT list


-- searching spec
searchSpec :: ([Integer] -> Integer -> Maybe Int) ->
              [Integer] -> Integer -> Bool
searchSpec f these this =
    List.elemIndex this these == f these this
    

-- horner's rule over polynomials on ints
hornersRuleSpec :: [Integer] -> Integer -> Bool
hornersRuleSpec list val =
    let indices = [0..(length list)]
    in sum [ai * val ^ i | (ai, i) <- list `zip` indices]
       == C1.hornersRule list val


-- inversions of a sequence
inversionsSpec :: [Integer] -> Bool
inversionsSpec list = let pair = C1.inversions list
                      in and $ fmap (isInversion list) pair
    where isInversion this (i,j) = this !! i > this !! j


-- for binary search we need to test slightly differently
-- cannot really rely on Data.List.elemIndex ?? I think
doBinarySearch :: [Integer] -> Integer -> Bool
doBinarySearch this that =
    case mindex of
        Nothing -> that `notElem` this
        Just n  -> that == (_sorted !! n)

    where _sorted = List.sort this
          mindex  = C1.binarySearch _sorted that


main :: IO ()
main = hspec $ do

    describe "Insertion Sort test over integers :" $
        it "\t this implementation is equivalent to that in Data.List :" $
            quickCheck $ sortSpec C1.insertionSort

    describe "\nMerge Sort test over integers :" $
        it "\t this implementation is equivalent to that in Data.List" $
            quickCheck $ sortSpec C1.mergeSort

    describe "\nInsertion Sort test over integers with ordering :" $
        it "\t this implementation is equivalent to that in Data.List" $
            quickCheck sortSpecOrdering

    describe ("\nFind a random Integer from a random [Integer] not necessarily contained"
             ++"\n Using resursive find in sequence :") $
        it "\t should be functionally equivalent to List.elemIndex" $
            quickCheck $ searchSpec C1.findInSequence

    describe ("\nFind a random Integer from a random [Integer] not necessarily contained"
             ++"\n Using binary search :") $
        it "\t should be functionally equivalent to List.elemIndex" $
            quickCheck doBinarySearch

    describe "\nHorners rule over integers :" $
        it "\t the evaluation should be identical to the obvious sum" $
            quickCheck hornersRuleSpec

    describe "\nInversions of an Array of integers" $
        it "\t a pair of integers i,j is an inversion iff i < j && A[i] > A[j]" $
            quickCheck inversionsSpec
