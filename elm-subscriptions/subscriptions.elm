-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (every, millisecond, Time)

--
-- Exercise 3
-- This exercise is about responding to subscriptions at a fixed interval
-- Your job is to
-- 1: Start a subscription every millisecond
-- 2: Update the model when the subscription arrive in the update functions
-- 3: Set the width of the second progress-bar in the view (by correctly
--    updating the 'progress' variable in line 51) to go from 0 to 100 once
--    every 5 seconds. It should start with a width of 0 pixels and during the
--    next 50 millisecond grow to a width of 1 and so on. When 5000 milliseconds
--    have passed the width should reset to 0.
--

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = Time

init : (Model, Cmd Msg)
init =
  (0, Cmd.none)

-- UPDATE
type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Tick newTickedTime -> (newTickedTime, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond Tick

-- VIEW
view : Model -> Html Msg
view model =
  let
    progress = toFloat (round (Time.inMilliseconds model) % 5000) / 5000 * 100

  in
    svg [ viewBox "0 0 120 120", width "500px" ]
      [ rect [x "10", y "10", width "100", height "6", rx "2", ry "2" ] []
      , rect [x "10", y "10", width (toString progress), height "6", rx "2", ry "2", fill "#AAA"] []
      ]