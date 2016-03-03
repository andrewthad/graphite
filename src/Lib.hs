module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Orientation = Up | Down | Left | Right

-- type Graphite a = Orientation -> Double -> Double -> a -> Html

-- data Graphite a = Graphite
--   { grOrientation :: Orientation
--   , grX           :: Double
--   , grY           :: Double
--   , grFunc        :: [a] -> Html
--   , gr            :: Int
--   }

-- Let's say we have a double bar graph:
--
--      |
--      |
-- Ht   |          |    Bars by gender
--      |      ||  ||
--      |  ||  ||  ||
--      -----------------
--            Age

-- ex         :: Quantity
-- ex2        :: Gender -> Quantity
-- ex3        :: (Age,Gender) -> Quantity  OR  Age -> Gender -> Quantity
-- In a sense, singleBar consumes an `a`.
-- singleBar  :: (a -> Double) -> Thickness -> Orientation -> (a -> Diagram)
-- multi      :: Orientation -> (a -> Diagram) -> [x] -> (x ->      a) -> Diagram
-- multi      ::                                         (y -> x -> a) -> Diagram
-- multi'     :: Orientation -> ((Rec Identity rs -> a) -> Diagram) -> [x]
--                           -> (Rec Identity (x ': rs) -> a) -> Diagram
-- singleLine :: (a -> Double) -> Spacing -> Orientation -> ([a] -> Diagram)
-- many       :: (a -> Diagram) -> Spacing -> ([a] -> Diagram)
-- many       :: (a -> Diagram) -> Spacing -> [x] -> (x -> a) -> Diagram


