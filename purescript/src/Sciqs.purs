module Sciqs where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
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
    , chosenAnswer  :: Maybe AnswerId
    }

-- Type aliases
type QuestionId
  = Int

type Answer
  = String

type AnswerId
  = Int


data Action =
    NewGame                          -- start a new game
  | ClickAnswer QuestionId AnswerId  -- click on an answer
  | QuestionReceived Question        -- receive an AJAX response with a question


type State = {
  score              :: Int,
  questions          :: Array Question,
  waitingForQuestion :: Boolean -- are we waiting for an AJAX call to return?
}


-- | Start out with no questions.
initialState :: State
initialState = { score: 0, questions: [], waitingForQuestion: true }


-- | When you click on questionId answerId, we need to update the score,
-- | set the chosenAnswer field for that question, and set the
-- | waitingForQuestion flag.
answerClicked :: QuestionId -> AnswerId -> State -> State
answerClicked questionId answerId state =
  { score : newScore, questions: newQuestions, waitingForQuestion: true }
  where
    q = case state.questions `unsafeIndex` questionId of Question q' -> q'
    newScore =
      if q.correctAnswer == answerId then state.score + 1 else state.score
    answeredQuestion = Question $ q { chosenAnswer = Just answerId }
    newQuestions =
      fromJust $ updateAt questionId answeredQuestion state.questions

-- | we only want to *actually* add the question if we're waiting for it.
-- | otherwise, we just return the state as is
appendQuestion :: Question -> State -> State
appendQuestion question state =
  if state.waitingForQuestion
  then state { questions          = snoc state.questions question,
               waitingForQuestion = false }
  else state

-- How to update the state (and perform effects) for each action type.
update :: forall eff. Update (ajax    :: AJAX,
                              err     :: EXCEPTION,
                              console :: CONSOLE    | eff) State Action
update action state input =
  case action of
    -- for a NewGame action, we need to reset the state to the initialState
    -- and make an AJAX request for a new question
    NewGame ->
      { state: initialState
      , effects: [ requestQuestion ] }
    -- for a click on an answer, update the state with the clicked answer,
    -- and make an AJAX request for a new question
    ClickAnswer questionId answerId ->
      { state: answerClicked questionId answerId state
      , effects: [ requestQuestion ]
      }
    -- for the result of a new question AJAX call, try to update the state
    QuestionReceived question ->
      { state: appendQuestion question state
      , effects: [] }
  where
    -- AJAX boilerplate that I don't totally understand
    requestQuestion =
      launchAff $ do
        res <- get questionServiceUrl
        let question = readJSON res.response :: F Question
        liftEff $ case question of
          (Left err) -> log "Error parsing JSON!"
          (Right question) -> S.send input (singleton (QuestionReceived question))

main :: Effect Unit
main =
  launchAff_ do
    result <- AX.get ResponseFormat.string questionServiceUrl
    case result of
      Left err -> log $ "GET /api response failed to decode: " <> AX.printError err
      Right response -> case SimpleJSON.readJSON response.body of
        Right (r :: Question) -> do
          log $ "question: "       <> r.questionText
          log $ "answers: "        <> show r.answers
       -- log $ "correct answer: " <> (fromMaybe "unknown" $ index r.answers r.correctAnswer)
          log $ "correct answer: " <> show r.correctAnswer
          log $ "chosen answer: "  <> "Nothing"
        Left e -> log $ "Can't parse JSON. " <> show e

