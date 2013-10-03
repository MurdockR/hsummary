module Main where

import System.Environment
import Data.List
import qualified Data.Map as Map

type Paragraph = [Sentence]
type Sentence = String
type Word = String
type Score = Float
type SentenceNumber = Int
type ParagraphScores = Map.Map String Float


getUniqueWords :: Sentence -> [Word]
getUniqueWords = nub . words


sentencesIntersect :: Sentence -> Sentence -> Score
sentencesIntersect sentence1 sentence2 = do
    let uniqueWords   = (getUniqueWords sentence1, getUniqueWords sentence2)
        commonWords   = intersect (fst uniqueWords) (snd uniqueWords)
        averageLength = (fromIntegral ((length $ fst uniqueWords) + (length $ snd uniqueWords))) / 2
    (fromIntegral (length commonWords)) / averageLength


sentenceScores :: Paragraph -> SentenceNumber -> [Score]
sentenceScores [] _ = []
sentenceScores paragraph sentenceNumber = do
    let getSentence = last . fst . splitAt (sentenceNumber + 1)
        sentence = getSentence paragraph 
    map (sentencesIntersect sentence) $ paragraph \\ [sentence]


paragraphScores :: Paragraph -> ParagraphScores
paragraphScores paragraph = do
    let getSentence sentence = head . findIndices (==sentence) 
        getSentenceScores = sentenceScores paragraph . flip getSentence paragraph
        scoreMatrix = foldl' (\ scoreList sentence -> scoreList ++ [getSentenceScores sentence]) [] paragraph
        sentenceScoreTotal = map sum scoreMatrix
    Map.fromList $ zip paragraph sentenceScoreTotal 
    


-- | TODO : Does not currently generate a summary, only returns the
-- | scoresmap for the first paragraph
genSummary :: [ParagraphScores] -> String
genSummary = undefined

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (args !! 0)
    -- | This is a god awful way of getting paragraphs/sentences from the
    -- | input txt, only used for testing
    inputLines <- return $ lines contents
    putStrLn $ show (paragraphScores inputLines)
