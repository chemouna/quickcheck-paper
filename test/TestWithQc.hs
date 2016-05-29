module Main.Test where

import Test.QuickCheck

-- simple examples
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

prop_RevId xs = reverse xs == xs
  where types = xs::[Int]

-- conditional properties

ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

prop_Insert x xs = ordered xs ==> ordered (insert x xs)
  where types = x::Int

-- quantified properties
-- forAll <generator> $ \<pattern> -> <property>

prop_Insert2 x xs = forAll orderedList $ \xs -> ordered (insert x xs)
    where types = x::Int

-- Counting Trivial Cases
-- <condition> `trivial` <property>
-- prop_Insert3 x xs = ordered xs ==> null xs `trivial` ordered (insert x xs)
--    where types = x::Int

-- Classifying Test Cases
-- classify <condition> <string>$ <property>
prop_Insert4 x xs =
    ordered xs ==>
        classify (ordered (x:xs)) "at-head"$
        classify (ordered (xs++[x])) "at-tail"$
        ordered (insert x xs)
    where types = x::Int


-- Collecting Data Values
-- collect <expression>$ <property>
prop_Insert5 x xs =
    ordered xs ==> collect (length xs)$
                   ordered (insert x xs)
           where types = x::Int

-- data generators
-- choose :: Random a => (a, a) -> Gen a
-- do i <- choose(0, length xs - 1)
--    return (xs!!i)

