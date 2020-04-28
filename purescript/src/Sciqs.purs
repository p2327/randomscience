module Sciqs where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Simple.JSON as SimpleJSON

-- Server url
questionServiceUrl :: String
questionServiceUrl = "http://localhost:8080/question"

-- Question type to create a Foreign type
type Question
  = { questionText :: String
    , answers :: Array String
    , correctAnswer :: Int
    --chosenAnswer  :: Maybe AnswerId
    }

-- Type aliases
type QuestionId
  = Int

type Answer
  = String

type AnswerId
  = Int

main :: Effect Unit
main =
  launchAff_ do
    result <- AX.get ResponseFormat.string questionServiceUrl
    case result of
      Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
      Right response -> case SimpleJSON.readJSON response.body of
        Right (r :: Question) -> do
          log $ "question: " <> r.questionText
          log $ "answers: " <> show r.answers
          log $ "correct answer: " <> (fromMaybe "unknown" $ index r.answers r.correctAnswer)
        Left e -> log $ "Can't parse JSON. " <> show e
