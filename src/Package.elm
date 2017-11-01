module Package exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Helpers
import Module exposing (Module)

type alias Dependency =
  { username: String
  , repository: String
  , version: String
  }

type alias Package =
  { root: String
  , modules: Dict String Module
  , sources: List String
  , exposed: List String
  , withNative: Bool
  , withEffects: Bool
  , dependencies: List Dependency
  }

type alias PackageJson =
  { sources: Maybe (List String)
  , exposed: Maybe (List String)
  , withNative: Maybe Bool
  , dependencies: Maybe (List (String, String))
  }

fromJson: String -> PackageJson -> Package
fromJson path json =
  { root = path
  , modules = Dict.empty
  , sources = json.sources |> Maybe.withDefault []
  , exposed = json.exposed |> Maybe.withDefault []
  , withNative = json.withNative |> Maybe.withDefault False
  , withEffects = False
  , dependencies =
      json.dependencies
      |> Maybe.withDefault []
      |> List.filterMap (\(name, version) ->
        case String.split "/" name of
          [] ->
            let a = Debug.log "Wrong dependency" name in Nothing
          z :: [] ->
            let a = Debug.log "Wrong dependency" name in Nothing
          username :: rest ->
            Just { username = username, repository = String.join "/" rest, version = version }
      )
  }

decoderJson: Decoder PackageJson
decoderJson =
  Decode.map4 PackageJson
    (Decode.maybe <| Decode.field "source-directories" <| Decode.list Decode.string)
    (Decode.maybe <| Decode.field "exposed-modules" <| Decode.list Decode.string)
    (Decode.maybe <| Decode.field "native-modules" Decode.bool)
    (Decode.maybe <| Decode.field "dependencies" <| Decode.keyValuePairs Decode.string)

encodeDependency: Dependency -> Encode.Value
encodeDependency dep =
  Encode.object
  [ ("username", Encode.string dep.username)
  , ("repository", Encode.string dep.repository)
  , ("version", Encode.string dep.version)
  ]

encode: Package -> Encode.Value
encode p =
  Encode.object
  [ ("root", Encode.string p.root)
  , ("modules", Helpers.encodeDict Module.encode p.modules)
  , ("sources", Helpers.encodeList Encode.string p.sources)
  , ("exposed", Helpers.encodeList Encode.string p.exposed)
  , ("withNative", Encode.bool p.withNative)
  , ("withEffects", Encode.bool p.withEffects)
  , ("dependencies", Helpers.encodeList encodeDependency p.dependencies)
  ]
