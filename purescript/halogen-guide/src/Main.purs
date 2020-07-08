module Main where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = Maybe Int
data Action = Increment | Decrement | Regenerate

component :: forall query i o m. MonadEffect m => H.Component HH.HTML query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ = Nothing

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      value = maybe "No number generated yet" show state
    in
     HH.div_
     [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
     , HH.div_ [ HH.text value ]
     , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
     , HH.button [ HE.onClick \_ -> Just Regenerate ] [ HH.text "Generate new number" ]
     ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Increment -> H.modify_ $ map (_ + 1)
    Decrement -> H.modify_ $ map (_ - 1)
    Regenerate -> do
      newNumber <- H.liftEffect random
      H.modify_ $ const $ Just $ round $ newNumber * 10.0
