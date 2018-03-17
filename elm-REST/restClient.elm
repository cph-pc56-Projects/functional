import Html exposing (..)
import Html.Events exposing (..)
import WebSocket

-- main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model = List String

type Msg
  = Send
  | Receive String


init : (Model, Cmd Msg)
init =
  (["Welcome!"], Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Send ->
      (model, WebSocket.send "ws://localhost:1234/hello" "Hello, server!")

    Receive message ->
      ((List.append model [message]), Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:1234/hello" Receive


view : Model -> Html Msg
view model =
  let
    renderMessage msg =
      div [] [ text msg ]
  in
    div []
      [ div [] (List.map renderMessage model)
      , button [onClick Send] [text "Send message to server!"]
      ]