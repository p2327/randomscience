module Main where


import Data.Array
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as B
import Halogen.VDom.Driver (runUI)
import Simple.JSON as SimpleJSON
import Utils (css, classes)

-- | Server url
questionServiceUrl :: String
--questionServiceUrl = "http://localhost:8080/question"
questionServiceUrl = "https://randomscience.ew.r.appspot.com/"

-- | Define a question type to match the JSON response from the server
type Question
  = { questionText  :: String
    , answers       :: Array String
    , correctAnswer :: Int
    }

-- | Below we use Purescript's algebraic data types (ADTs).
-- | ADTs use Algebra to define the total number of values a given type can have.

-- | Actions are raised within components to trigger evaluation
-- | causing a change within a component or an effect (i. e. an update)
-- | Effects and changes are determined in the handleAction function
data Action
  = NewGame                            -- Start a new game
  | ClickAnswer Int                    -- Click on the n-th answer
  | NextQuestion                       -- Get a server response with a question


-- | Status tracks if the app has a question and an answer
data Status
  = WaitingForQuestion                 -- Waiting for server response (a question)
  | HaveQuestion Question (Maybe Int)  -- The current question and a Maybe value storing what answer has been clicked
  | Failure String                     -- Failed to get server response


-- | The state of our application represented in a record type
-- | It contains Status, which is defined as an ADT
type State
  = { score         :: Int             -- Keeps track of the score
    , status        :: Status          -- Keeps track of the status of question and answers
    }

-- | Start out with no questions
initialState :: State
initialState = { score: 0, status: WaitingForQuestion }

-- | Helper function for creating buttons that trigger an action.
mkButton :: forall a. String -> Action -> HH.HTML a Action
mkButton str act =
  HH.button
    [ HE.onClick \_ -> Just act
    , HP.type_ ButtonButton
    , css "button"
    ]
    [ HH.text str ]

-- | Render represents the state as output
render :: forall m. State -> H.ComponentHTML Action () m
render s =
  let
    questionBlock = case s.status of
      WaitingForQuestion -> HH.text "Loading..."
      HaveQuestion question maybeAnswer ->
        let
          answerSummary = case maybeAnswer of
            Nothing -> []
            Just idx ->
              [ HH.div
                [ classes ["answerSummary", "text" ] ]
                  [ HH.text
                      $ if idx == question.correctAnswer then
                          "Correct!"
                        else
                          "Incorrect, the right answer is " <> show (fromMaybe "" (question.answers !! question.correctAnswer))
                  ]
              ]
        in
          HH.div
            [ css "text" ]
            $ [ HH.text question.questionText
              ]
            <> mapWithIndex (\idx txt -> HH.div [ css "answerBlock" ] [ mkAnswerButton idx txt ]) question.answers
            <> answerSummary
            <> nextQuestionBtn                               -- Append next question button here
      Failure str -> HH.text $ "Failed: " <> str

    mkAnswerButton idx str =                                 -- Function to render a render a button that:
      let
        style = case s.status of                             -- has different style depending on HaveQuestion status
          HaveQuestion q (Just i) ->                         
            if i == idx then                                 -- and, if a button is clicked (i == idx) 
              if q.correctAnswer == i then                   -- whether the clicked button matches the answer
                [ B.btnLg
                , B.btnSuccess
                , B.btnBlock 
                ]
              else                                           -- or not...
                [ B.btnLg
                , B.btnDanger
                , B.btnBlock 
                ]
            else                                             -- Here we handle unclicked buttons
              [ B.btnLg
              , B.btnOutlineSecondary
              , B.btnBlock
              , HH.ClassName "disabled" 
              ]                                              
          _ -> [ B.btnLg                                     -- Render buttons where answer has not been clicked yet
               , B.btnOutlineDark                            -- i.e. status is not HaveQuestion q (Just i)
               , B.btnBlock 
               ]                                             

        act = case s.status of                               -- Our action: on click, triggers ClickAnswer 
          HaveQuestion q Nothing -> Just $ ClickAnswer idx   -- if we have a question with no answer       
          _ -> Nothing

      in
        HH.button
          [ HE.onClick \_ -> act
          , HP.classes style
          ]
          [ HH.text str ]
    
    nextQuestionBtn = case s.status of                       -- Renders the next question button       
      HaveQuestion q (Just i) ->                             -- only when HaveQuestion answer is not Nothing
            [ HH.button
                [ css "button" 
                , HE.onClick \_ -> Just NextQuestion
                ]
                [ HH.text "Next Question" ]
            ]
      _ -> []

  in
    HH.div 
        [ css "wrapper" ]
          $ [ HH.div 
              [ classes [ "header", "sidebar" ] ]                              
                [ HH.div_ 
                    [ HH.div
                      [ css "h1" ] 
                        [ HH.text "Random Science" ]
                      , HH.div_
                          [ HH.div
                            [ css "text"]
                              [ HH.text "A markov-chain powered quiz game"] 
                          ]
                      , HH.div_
                        [ mkButton "New Game" NewGame ]
                      , HH.div
                          [ css "text" ]
                            [ HH.div_ 
                            [ HH.text $ "Score: " <> show s.score ]      
                            ]
                      ]
                  ]
              , HH.div 
                [ css "top" ]
                  [ HH.div_                   
                    [ HH.text "" ]
                  ]
              , HH.div 
                  [ css "content" ]                        
                      [ questionBlock ]
              , HH.div
                  [ classes [ "footer", "smallprint" ] ]
                    [ HH.div_ 
                      [ HH.text "Special thanks to " 
                      , HH.a 
                          [ HP.href "https://github.com/milesfrain"] 
                          [ HH.text "milesfrain " ]
                      , HH.text "and "
                      , HH.a 
                          [ HP.href "https://github.com/thomashoneyman"] 
                          [ HH.text "thomashoneyman " ]
                      ]
                    ]
          ]


-- | Shows how to use actions to update the component's state
-- | Determine effects based upon which action has been raised
-- | Once state has been modified, a re-render is triggered
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  -- | A new game resets the score and triggers NextQuestion 
  NewGame -> do
    H.modify_ \s -> s { score = 0 }
    handleAction NextQuestion
  -- | Clicking an answer updates HaveQuestion and score with 1 or 0 points
  ClickAnswer idx -> do
    H.modify_ \s -> case s.status of
      HaveQuestion q _ ->
        let
          points = if idx == q.correctAnswer then 1 else 0
        in
          s { status = HaveQuestion q (Just idx), score = s.score + points }
      _ -> s { status = Failure $ "Somehow clicked idx " <> show idx <> " when not in question display state" }
  -- | Next question fetches data from the server via an AJAX request
  NextQuestion -> do
    H.modify_ \s -> s { status = WaitingForQuestion }
    result <- liftAff $ AX.get ResponseFormat.string questionServiceUrl
    case result of
      Left err -> H.modify_ \s -> s { status = Failure $ "GET /api response failed to decode: " <> AX.printError err }
      Right response -> case SimpleJSON.readJSON response.body of
        Right (r :: Question) -> do
          H.modify_ \s -> s { status = HaveQuestion r Nothing }
        Left e -> H.modify_ \s -> s { status = Failure $ "Can't parse JSON. " <> show e }

-- | The Component type combines state management and rendering information
-- | mkComponent takes a record used to describe relevant values and functions for a component operation
component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const initialState       -- const is used as the component initializes with a predifined state 
    , render                                 -- Tells the component how to render for a given state
    , eval:                                  -- Handles effectful actions
        H.mkEval                             -- Uses the convenience function mkEval
          $ H.defaultEval                    -- With a modified defaultEval
              { handleAction = handleAction
              , initialize = Just NewGame
              }
    }

-- | The main function gets the body element
-- | and installs the component using runUI
main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
