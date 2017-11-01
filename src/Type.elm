module Type exposing (..)

import Dict
import Json.Encode as Encode
import Elm.Docs.Type as Docs
import Parser exposing (Parser)

import Helpers

{-| Represent Elm types as values! Here are some examples:

    Int            ==> Type "Int" []

    a -> b         ==> Lambda (Var "a") (Var "b")

    ( a, b )       ==> Tuple [ Var "a", Var "b" ]

    Maybe a        ==> Type "Maybe" [ Var "a" ]

    { x : Float }  ==> Record [("x", Type "Float" [])] Nothing
-}
type alias Type = Docs.Type

parser: Parser Type
parser =
  Docs.tipe

{-
Flatten a "recursive" lambda type to a list of types

  Original code source:
  Int -> String -> Bool

  Parsed type:
  Lambda (Type "Int" []) (Lambda (Type "String" []) (Type "Bool" []))

  Flatten version:
  [ Type "Int" [], Type "String" [], Type "Bool" []]

-}
flatten: Type -> List Type
flatten tipe =
  case tipe of
    Docs.Lambda from to -> from :: (flatten to)
    _ -> [ tipe ]


encode: Type -> Encode.Value
encode tipe =
  (
    case tipe of
      Docs.Var str ->
        ("Var", [("name", Encode.string str)])
      Docs.Lambda from to ->
        ("Lambda", [("from", encode from), ("to", encode to)])
      Docs.Tuple values ->
        ("Tuple", [("values", Helpers.encodeList encode values)])
      Docs.Type name args ->
        ("Type", [("name", Encode.string name), ("args", Helpers.encodeList encode args)])
      Docs.Record fields ext ->
        ("Record"
        , [ ("fields", fields |> Dict.fromList |> Helpers.encodeDict encode)
          , ("extension", Helpers.encodeMaybe Encode.string ext)
          ]
        )
  )
  |> (\(name, values) ->
    Encode.object <| ("name", Encode.string name) :: values
  )
