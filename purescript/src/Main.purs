module Main where


import Data.Array
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Color.Scale (cssColorStops)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (ClassName(..))
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
--questionServiceUrl = "http://54.174.99.38/question"
questionServiceUrl = "https://randomscience.ew.r.appspot.com/"

-- | Question type matches the JSON response with a record
type Question
  = { questionText  :: String
    , answers       :: Array String
    , correctAnswer :: Int
    }

data Action
  = NewGame                            -- Start a new game
  | ClickAnswer Int                    -- Click on an answer
  | NextQuestion                       -- Receive an AJAX response with a question

data Status
  = WaitingForQuestion                 -- Waiting for server response
  | HaveQuestion Question (Maybe Int)  -- Which answer has been clicked for the current question
  | Failure String                     -- Failed to get server response

type State
  = { score         :: Int             
    , status        :: Status
    , buttonClicked :: Maybe Int       -- What button has been clicked
    }

-- | Start out with no questions.
initialState :: State
initialState = { score: 0, status: WaitingForQuestion, buttonClicked: Nothing }

-- | Helper function for creating buttons that trigger an action.
mkButton :: forall a. String -> Action -> HH.HTML a Action
mkButton str act =
  HH.button
    [ HE.onClick \_ -> Just act
    --, HP.classes [ B.btnLg, B.btnOutlineInfo ]
    , HP.type_ ButtonButton
    , css "button"
    ]
    [ HH.text str ]

-- | Shows how to add event handling.
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

    mkAnswerButton idx str =                                 -- Renders a button that:
      let
        style = case s.status of                             -- has different style depending on HaveQuesiton state
          HaveQuestion q (Just i) ->                         -- and if the clicked answer matches the correct one
            if i == idx then
              if q.correctAnswer == i then
                [ B.btnLg, B.btnSuccess, B.btnBlock ]
              else
                [ B.btnLg, B.btnDanger, B.btnBlock ]
            else                      
              [ B.btnLg, B.btnOutlineSecondary, B.btnBlock, HH.ClassName "disabled" ] -- Buttons not clicked
          _ -> [ B.btnLg, B.btnOutlineDark, B.btnBlock ]     -- If no answer has been clicked

        act = case s.status of                               -- On click, triggers ClickAnswer 
          HaveQuestion q Nothing -> Just $ ClickAnswer idx          
          _ -> Nothing
      in
        HH.button
          [ HE.onClick \_ -> act
          , HP.classes style
          ]
          [ HH.text str ]
    
    nextQuestionBtn = case s.status of                       -- Renders the next question button       
      HaveQuestion q (Just i) ->                             -- only when HaveQuestion is not Nothing
            [ HH.button
                --[ HP.classes [ B.btnLg, B.btnInfo ]
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
              [ classes [ "header", "sidebar" ] ]                              -- See https://getbootstrap.com/docs/4.4/layout/grid/
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
                  [ css "content" ]                        -- How to show a border for this column ?
                      [ questionBlock ]
              , HH.div
                  [ classes [ "footer", "text" ] ]
                    [ HH.a 
                      [ HP.href "https://www.w3schools.com"] 
                      [ HH.text "I am the footer" ]
                    ]
            ]

            

    


    
        
                    
              
    
    

    
    
        



          {-
                            , HH.div 
                    [ HP.classes [ B.col8 ] ] 
                      [ HH.text "Placeholder for explanatory text"]
                    ]      

                                      , HH.div 
                    [ HP.classes [ B.col8 ] ]                        
                      [ questionBlock ]
                  ]                   
  
      -}
    
      -- <> nextQuestionBtn                -- Append this button ouside the div
                                           -- Moved to questionBlock to render under the questions
{- 
  - Render NewGame button, score and nextQuestionBtn in second column
  - Render a sidebar / line to separate the columns
-}

-- | Shows how to use actions to update the component's state
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  NewGame -> do
    H.modify_ \s -> s { score = 0 }
    handleAction NextQuestion
  ClickAnswer idx -> do
    H.modify_ \s -> case s.status of
      HaveQuestion q _ ->
        let
          points = if idx == q.correctAnswer then 1 else 0
        in
          s { status = HaveQuestion q (Just idx), score = s.score + points }
      _ -> s { status = Failure $ "Somehow clicked idx " <> show idx <> " when not in question display state" }
  NextQuestion -> do
    H.modify_ \s -> s { status = WaitingForQuestion }
    result <- liftAff $ AX.get ResponseFormat.string questionServiceUrl
    case result of
      Left err -> H.modify_ \s -> s { status = Failure $ "GET /api response failed to decode: " <> AX.printError err }
      Right response -> case SimpleJSON.readJSON response.body of
        Right (r :: Question) -> do
          H.modify_ \s -> s { status = HaveQuestion r Nothing }
        Left e -> H.modify_ \s -> s { status = Failure $ "Can't parse JSON. " <> show e }


component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just NewGame
              }
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
