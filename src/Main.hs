module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower, toUpper)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ lines dict

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  wordCandidates <- allWords
  return $ filter isGameWord wordCandidates
  where isGameWord word = length word <= maxWordLength && length word >= minWordLength

randomWord :: WordList -> IO String
randomWord wordList = do
  randomWordIndex <- randomRIO (0, length wordList - 1)
  return $ wordList !! randomWordIndex

gameWord :: IO String
gameWord = gameWords >>= randomWord

data Hangman = Hangman {
    word             :: String
  , filledCharacters :: [Maybe Char]
  , triedCharacters  :: String
}

instance Show Hangman where
  show game =
    (intersperse ' ' $ fmap renderPuzzleChar (filledCharacters game))
    ++ "\nGuessed so far: " ++ triedCharacters game ++
    " (" ++ show (incorrectTries game) ++ " incorrect)"

freshHangman word = Hangman {
    word = word
  , filledCharacters = replicate (length word) Nothing
  , triedCharacters = []
}

charInWord :: Hangman -> Char -> Bool
charInWord hangman userTry = userTry `elem` word hangman

alreadyTried :: Hangman -> Char -> Bool
alreadyTried hangman userTry = userTry `elem` triedCharacters hangman

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just character) = character

incorrectTries :: Hangman -> Int
incorrectTries hangman =
  length $ filter (\c -> c `notElem` word hangman) (triedCharacters hangman)

fillInCharacter :: Hangman -> Char -> Hangman
fillInCharacter hangman userTry
  | alreadyTried hangman userTry = hangman
  | not $ charInWord hangman userTry =
      Hangman {
          word = word hangman
        , filledCharacters = filledCharacters hangman
        , triedCharacters = userTry : triedCharacters hangman
      }
  | otherwise =
      Hangman {
          word = word hangman
        , filledCharacters = map (snd . matchZipped) filledZip
        , triedCharacters = userTry : triedCharacters hangman
      }
    where filledZip = zip (word hangman) (filledCharacters hangman)
          matchZipped (wordChar, Nothing)
            | toLower wordChar == userTry = (wordChar, Just wordChar)
            | otherwise = (wordChar, Nothing)
          matchZipped x = x

handleTry :: Hangman -> Char -> IO Hangman
handleTry hangman userTry = do
  putStrLn $ "Your guess was: " ++ [userTry]
  case (charInWord hangman userTry
      , alreadyTried hangman userTry) of
    (_, True) -> do
      putStrLn $ userTry : " already guessed, try something else."
      return $ fillInCharacter hangman userTry
    (True, _) -> return $ fillInCharacter hangman userTry
    (False, _) -> do
      putStrLn $ userTry : " is a miss! Try again."
      return $ fillInCharacter hangman userTry

maxIncorrectTries :: Int
maxIncorrectTries = 10

gameLost :: Hangman -> IO ()
gameLost hangman =
  if incorrectTries hangman > maxIncorrectTries then
    do
      putStrLn $ "Game over! The word was: " ++ map toUpper (word hangman)
      exitSuccess
  else
    return ()

gameWon :: Hangman -> IO ()
gameWon hangman =
  if all isJust (filledCharacters hangman) then
    do
      putStrLn $ "Congrats! You've guessed the word " ++ map toUpper (word hangman) ++ " in " ++
        show (length $ triedCharacters hangman) ++ " tries."
      exitSuccess
  else
    return ()

runGame :: Hangman -> IO ()
runGame hangman = forever $ do
  gameWon hangman
  gameLost hangman
  putStrLn $ "Current hangman session is: " ++ show hangman
  putStr "Try a letter: "
  guess <- getLine
  case guess of
    [userTry] -> handleTry hangman userTry >>= runGame
    _ -> putStrLn "Your try must be a single character."

main :: IO ()
main = do
  word <- gameWord
  runGame $ freshHangman word
