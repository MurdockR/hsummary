module Main where

import System.IO
import System.Directory
import System.Environment
import Data.List
import qualified Data.Map as Map

sentenceScores :: [String] -> Int -> [Float]
sentenceScores [] _ = []
sentenceScores p sn = do
    let s = (\ paragraph sentence -> last . fst $ splitAt (sentence+1) paragraph) p sn
    map (sentencesIntersect s) $ p \\ [s]


paragraphScores :: [String] -> Map.Map String Float
paragraphScores p = do
    let scores = foldl' (\ scores sentence -> scores ++ [(sentenceScores p (head $ findIndices (==sentence) p))]) [] p
    Map.fromList $ zip p $ map (\ scoreList -> foldl' (+) 0.0 scoreList) scores


sentencesIntersect :: String -> String -> Float
sentencesIntersect sen1 sen2 = do
    let s1          = nub $ words sen1
        s2          = nub $ words sen2
        commonWords = intersect s1 s2
        averageLength = (fromIntegral ((length s1)+length s2)) / 2
    (fromIntegral (length commonWords)) / averageLength


-- | TODO : Does not currently generate a summary, only returns the
-- | scoresmap for the first paragraph
genSummary :: [Map.Map String Float] -> String
genSummary [scoresMap] = undefined


main = do
    args <- getArgs
    contents <- readFile (args !! 0)
    -- | This is a god awful way of getting paragraphs/sentences from the
    -- | input txt, only used for testing
    lines <- return $ lines contents
    putStrLn $ show (paragraphScores lines)
