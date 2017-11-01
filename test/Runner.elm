port module Runner exposing (..)

import Task exposing (Task)
import Json.Encode as Encode
import SyntaxParser
import Package exposing (Package)

type Msg
  = Analyze String
  | Analyzed (Result String Package)

type alias Model = {}

main: Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init: (Model, Cmd Msg)
init = {} ! []

subscriptions: Model -> Sub Msg
subscriptions model =
  analyze Analyze

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Analyze path ->
      let a = Debug.log "Parse" path in
      model ! [ Task.attempt Analyzed (SyntaxParser.parse path) ]

    Analyzed res -> case res of
      Err err -> model ! [ done (Encode.string err) ]
      Ok package -> model ! [ done (Package.encode package) ]

port analyze: (String -> msg) -> Sub msg

port done: Encode.Value -> Cmd msg
