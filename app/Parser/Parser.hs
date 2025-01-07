module Parser where
import Text.Parsec as P
import Text.Parsec.String (Parser)
import Data.List.Split (chunksOf)
import Data.Char (isDigit, digitToInt, ord)
import qualified Data.List.Extra as E

data Puzzle = Puzzle
  { size   :: Int
  , top    :: [Int]
  , bottom :: [Int]
  , left   :: [Int]
  , right  :: [Int]
  , prefilled  :: [((Int, Int), Int)]
  } deriving Show

-- converts URL into Puzzle
getPuzzle s =
  let
    info = (parseWithChar ('#') s) !! 1
    size = read ( head ( parseWithChar ':' (info)))::Int
    constr = map (\x -> case x of {"" -> 0; x -> read x :: Int }) $ parseWithChar ('/') (head $ parseWithChar ',' $ drop 2 $ info)
    [top, bottom, left, right] = chunksOf size constr
    board = case ((parseWithChar ',' info) E.!? 1) of {Just x -> x; _ -> ""}
  in Puzzle size top bottom left right (parseBoard size board)


-- splits a String on a given Char 
parseWithChar :: Char -> String -> [String]
parseWithChar c s = case (P.parse (splitOn c) "" s) of
            Left err -> ["Fehler"]
            Right xs -> xs

-- get the already set entries
parseBoard :: Int -> [Char] -> [((Int, Int), Int)]
parseBoard size n = fst $ foldl (constructList size) ([],0) n

-- helper function for parseBoard
constructList :: Int -> ([((Int, Int), Int)], Int) -> Char -> ([((Int, Int), Int)], Int) 
constructList size (xs, acc) i = case (isDigit i) of
  True -> ((((div acc size), (mod acc size)),(digitToInt i)):xs, acc+1)
  False -> case i of 
    '_' -> (xs, acc)
    x -> (xs, (ord x) + acc - 96)

-- configure the Parser for splitting String on Char
splitOn :: Char -> Parser [String]
splitOn n= sepBy (many (noneOf [n])) (char n)
