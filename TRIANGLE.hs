-- Copyright (c) 2024 Serafín


module TRIANGLE where


import qualified Data.Set as S


data Graph = G (S.Set Int) (S.Set (S.Set Int))
    deriving (Show)



-- | Checks if there exists a triangle in a given graph.
-- A triangle is a set of three vertices in a graph that are pairwise connected by edges.
-- 
-- The function takes a 'Graph' as input and returns 'True' if a triangle exists, and 'False' otherwise.
-- 
-- The 'Graph' type is defined as follows:
-- 
-- @
-- data Graph = G (S.Set Int) (S.Set (Int, Int))
-- @
-- 
-- The first argument of the 'Graph' constructor is a set of vertices, represented by integers.
-- The second argument is a set of edges, represented by pairs of vertices.
-- 
-- The function uses the 'cn3' function to generate all possible sets of three vertices from the given graph -> Combinations(|vs|,3).
-- It then checks if any of these sets satisfy the condition for a triangle, which is that all three pairs of vertices are connected by edges.
-- 
-- The 'cn3' function returns a set of all possible combinations of three vertices from the given set of vertices.
-- 
-- The 'condition' function checks if a given set of three vertices forms a triangle by checking if all three pairs of vertices are present in the set of edges.
-- 
-- The function uses the 'any' function to check if there exists any set of three vertices that satisfy the condition for a triangle.
-- 
-- The function returns 'True' if a triangle exists, and 'False' otherwise.
triangle :: Graph -> Bool
triangle (G vs es) = any condition cn3List
    where 

        -- | Generate a set of sets with three elements containing all
        -- combinations of three elements in a given set.
        cn3 :: S.Set Int -> S.Set (S.Set Int)
        cn3 s = S.fromList [S.fromList [a, b, c] | a <- l, b <- l, c <- l, a /= b, b /= c, a /= c]
            where l = S.toList s

        cn3List = S.toList $ cn3 vs

        condition :: S.Set Int -> Bool
        condition s = (S.member t1 es) && (S.member t2 es) && (S.member t3 es)
            where 
                [a, b, c] = S.toList s
                t1 = S.insert a $ S.insert b S.empty
                t2 = S.insert b $ S.insert c S.empty
                t3 = S.insert c $ S.insert a S.empty



-- Examples

main :: IO ()
main = do
    
    print $ triangle (G (S.fromList [1, 2, 3]) (tuplesToSets [(1, 2), (2, 3), (1, 3)])) -- True
    print $ triangle (G (S.fromList [1, 2, 3, 4, 5, 6]) (tuplesToSets [(1, 3), (3,4), (2,4), (5,6), (4,5), (4,6)])) -- True
    print $ triangle (G (S.fromList [1, 2, 3, 4, 5, 6]) (tuplesToSets [(1, 3), (3,4), (2,4), (5,6), (4,5)])) -- False
    
    where 

        -- | Converts a list of tuples into a set of sets of integers.
        --
        -- Given a list of tuples, where each tuple contains two integers, this function
        -- converts each tuple into a set of integers and then returns a set of these sets.
        --
        -- Example:
        --
        -- >>> tuplesToSets [(1, 2), (3, 4), (5, 6)]
        -- fromList [fromList [1,2],fromList [3,4],fromList [5,6]]
        --
        -- The resulting set contains three sets, where each set represents a tuple from the input list.
        tuplesToSets :: [(Int, Int)] -> S.Set (S.Set Int)
        tuplesToSets = S.fromList . map (\(a, b) -> S.fromList [a, b])
