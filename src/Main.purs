module Main where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (wrap)
import React as R
import React.DOM as R
import React.DOM.Props as RP
import Thermite as T

type State = Int

data Action = Increment | Decrement

initialState :: State
initialState = 42

render :: T.Render State _ Action
render dispatch _ state _ =
  [ R.h1' [ R.text "Lesson 3 - Async" ]
  , R.p' [ R.text "The state is: "
         , R.text (show state)
         ]
  , R.p [ RP.className "btn-group" ] 
        [ R.button [ RP.className "btn btn-success"
                   , RP.onClick \_ -> dispatch Increment 
                   ]
                   [ R.text "Increment" ]
        , R.button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> dispatch Decrement
                   ]
                   [ R.text "Decrement" ]
        ]
  , R.p'  [ R.text "Go to "
          , R.a [ RP.href "?gist=1b0ef044e8b4af58c62ea42400d7ba7d&backend=thermite"
                , RP.target "_top"
                ]
                [ R.text "Lesson 4" ]
          , R.text "."
          ]
  ]

-- This handler waits for half a second before incrementing or 
-- decrementing the state.
--
-- Since the coroutine type is a monad transformer, we can
-- 'lift' computations in the 'Aff' monad, such as a delay,
-- before emitting values.

performAction :: T.PerformAction _ State _ Action
performAction Increment _ _ = void do
  lift (delay (wrap 500.0))
  T.modifyState \state -> state + 1
performAction Decrement _ _ = void do
  lift (delay (wrap 500.0))
  T.modifyState \state -> state - 1

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

main = T.defaultMain spec initialState
