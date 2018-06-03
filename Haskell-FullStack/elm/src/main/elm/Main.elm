import Html exposing (..)
-- import Html.Attributes exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (..)
import Http exposing (jsonBody)
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (toInt)

-- import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:3003/member/" ++ action

-- MODEL

type alias Member =
    { id: Int
    , name: String
    , email: String
    }

decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
    (Decode.at ["id"] Decode.int)
    (Decode.at ["name"] Decode.string)
    (Decode.at ["email"] Decode.string)

encodeMember : Member -> Encode.Value
encodeMember member =
    Encode.object
      [ ("id", Encode.int member.id)
      , ("name", Encode.string member.name)
      , ("email", Encode.string member.email)
      ]

type alias Model =
  { count : Int
  , member : Member
  , message : String
  , requestedMemberId : Int
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 {id = 0, name="looser", email="yes"} "No message" 1
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)
  | RequestMember
  | RequestedMemberId String
  | MemberReceived (Result Http.Error Member)
  | SetMemberId String
  | MemberName String
  | MemberEmail String
  | PostMember
  | MemberPosted (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    RequestMember ->
      if  model.requestedMemberId == 0
        then ( {model | message = "Can't request member with id 0"}, Cmd.none)
        else (model, getMember model.requestedMemberId)

    RequestedMemberId inputId ->
       case String.toInt inputId of
           Ok parsedId ->
               (model |> setRequestedMemberId parsedId
               , Cmd.none )
           Err _ ->
               (model |> setRequestedMemberId 0 |> setMessage "Id was not a number, defualted to 0"
               , Cmd.none )


    MemberReceived (Ok newMember) ->
      ( { model | member = newMember }, Cmd.none)

    MemberReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

    SetMemberId newId ->
      case String.toInt newId of
        Ok parsedId ->
            (model.member |> setId parsedId |> asMemberIn model
            , Cmd.none )
        Err _ ->
            (model.member |> setId 0 |> asMemberIn model
               |> setMessage "Id was not a number, defualted to 0"
            , Cmd.none )

    MemberName newName ->
      (model.member |> setName newName |> asMemberIn model
      , Cmd.none )

    MemberEmail newEmail ->
      (model.member |> setEmail newEmail |> asMemberIn model
        , Cmd.none )

    PostMember ->
      ( model, postMember model )

    MemberPosted (Ok newMessage) ->
      ( { model | message = newMessage }, Cmd.none)

    MemberPosted (Err error) ->
      ( { model | message = toString error }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , hr [] []
    , h2 [] [text "Member"]
    , input [type_ "number", value (toString model.member.id), onInput SetMemberId] []
    , input [value model.member.name, onInput MemberName] []
    , input [value model.member.email, onInput MemberEmail] []
    , button [ onClick PostMember ] [ text "Post Member" ]
    , hr [] []
    , button [ onClick RequestMember ] [ text "Get Member" ]
    , input [type_ "number", value (toString model.requestedMemberId), onInput RequestedMemberId ] []
    , hr [] []
    , text model.message
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- SETTERS

setMessage : String -> Model -> Model
setMessage newMessage model =
    { model | message = newMessage }

setRequestedMemberId : Int -> Model -> Model
setRequestedMemberId newRequestedMemberId model =
    { model | requestedMemberId = newRequestedMemberId }

setId : Int -> Member -> Member
setId newId member =
    { member | id = newId }

asIdIn : Member -> Int -> Member
asIdIn =
    flip setId

setName : String -> Member -> Member
setName newName member =
    { member | name = newName }

asNameIn : Member -> String -> Member
asNameIn =
    flip setName

setEmail : String -> Member -> Member
setEmail newEmail member =
    { member | email = newEmail }

asEmailIn : Member -> String -> Member
asEmailIn =
    flip setEmail

setMember : Member -> Model -> Model
setMember newMember model =
    { model | member = newMember }


asMemberIn : Model -> Member -> Model
asMemberIn =
    flip setMember

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember: Int -> Cmd Msg
getMember id =
    Http.send MemberReceived (Http.get (url (toString id)) decodeMember)

postMember: Model -> Cmd Msg
postMember model =
    Http.send MemberPosted (Http.post (url "") (jsonBody <| encodeMember model.member) Decode.string)
