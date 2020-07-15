module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Foldable (for_)
import Data.Int (round)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.String (length)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = { n :: Maybe Int, username :: String, loading :: Boolean }
data Action = Increment | Decrement | Regenerate | SetUsername String | MakeRequest Event

component :: forall query i o m. MonadAff m => H.Component HH.HTML query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: i -> State
  initialState _ = { n: Nothing, username: "", loading: false }

  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      value = maybe "No number generated yet" show state.n
    in
     HH.div_
     [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
     , HH.div_ [ HH.text value ]
     , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
     , HH.button [ HE.onClick \_ -> Just Regenerate ] [ HH.text "Generate new number" ]
     , HH.form
       [ HE.onSubmit $ Just <<< MakeRequest ]
       [ HH.h1_ [ HH.text "Look up GitHub user" ]
       , HH.label_
         [ HH.div_ [ HH.text "Enter username:" ]
         , HH.input [ HP.value state.username, HE.onValueInput $ Just <<< SetUsername ]
         ]
       , HH.button [] [ HH.text "Fetch info" ]
       , HH.p_ [ HH.text (if state.loading then "Working..." else "") ]
       ]
     ]

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Increment -> H.modify_ \state -> state { n = map (_ + 1) state.n }
    Decrement -> H.modify_ \state -> state { n = map (_ - 1) state.n }
    Regenerate -> do
      newNumber <- H.liftEffect random
      H.modify_ _ { n = Just $ round $ newNumber * 10.0 }
    SetUsername s -> H.modify_ _ { username = s }
    MakeRequest e -> do
      H.liftEffect $ Event.preventDefault e
      username <- H.gets _.username
      H.modify_ _ { loading = true }
      response <- H.liftAff $ AX.get AXRF.string $ "https://api.github.com/users/" <> username
      H.modify_ _ { loading = false }
      for_ (map _.body response) \body -> H.modify_ _ { n = Just $ length body }
