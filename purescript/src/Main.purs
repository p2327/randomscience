module Main where

import Prelude
import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

-- | Our state type. Either the button is 'on' or 'off'.
type State
  = Boolean

-- | Our action type. It indicates the button's state should be inverted
data Action
  = Toggle

-- | Shows how to add event handling.
toggleButton :: forall m. State -> H.ComponentHTML Action () m
toggleButton isOn =
  let
    toggleLabel = if isOn then "ON" else "OFF"
  in
    HH.button
      [ HP.type_ ButtonButton
      , HP.class_ $ ClassName "button"
      , HE.onClick \_ -> Just Toggle
      ]
      [ HH.text $ "The button is " <> toggleLabel ]

-- | Shows how to use actions to update the component's state
handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> do
    modify_ \oldState -> not oldState

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const false
    , render: toggleButton
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
