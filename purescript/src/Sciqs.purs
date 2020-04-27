module Sciqs where
  
import Prelude

import Control.Monad.Except (runExcept)
import Foreign (F, Foreign, readString, readArray, readInt, readNullOrUndefined)
import Foreign.Index ((!)) -- Operator alias for Data.Foreign.Index.ix (left-associative / precedence 9)
import Data.Traversable (traverse)
import Utils.Value (foreignValue)
import Effect (Effect)
import Effect.Console (logShow)


-- Server url
questionServiceUrl :: String
questionServiceUrl = "http://localhost:8080/question"


-- Question type to create a Foreign type
type Question = {
    questionText  :: String,
    answers       :: Array String,
    correctAnswer :: Int
    --chosenAnswer  :: Maybe AnswerId
}


-- Type aliases
type QuestionId = Int
type Answer     = String
type AnswerId   = Int


-- Read JSON question into a Foreign type
readQuestion :: Foreign -> F Question
readQuestion value = do
  questionText  <- value ! "questionText"  >>= readString
  answers       <- value ! "answers"       >>= readArray >>= traverse readString
  correctAnswer <- value ! "correctAnswer" >>= readInt
  --chosenAnswer  <- value ! ""              >>= fromMaybe readNullOrUndefined Nothing
  --pure { questionText, answers, correctAnswer, chosenAnswer }
  pure { questionText, answers, correctAnswer }



main :: Effect Unit
main = do
    let question = """
        { "questionText": "Sample Question" 
        , "answers": ["Answer1", 
                      "Answer2", 
                      "Answer3", 
                      "Answer4"]
        , "correctAnswer": 1
        }
        """
    logShow $ runExcept $ readQuestion =<< foreignValue question