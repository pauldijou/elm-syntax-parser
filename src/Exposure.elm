module Exposure exposing (..)

import Json.Encode as Encode
import Parser exposing (Parser, (|.), (|=))
import Parser.LanguageKit as Parser
import Helpers

type Exposure
  = All
  | Subset { functions: List String, operators: List String, types: List String }

-- PARSER

parser: Parser Exposure
parser =
  Parser.oneOf
  [ Parser.map (\_ -> All) (
      Parser.succeed ()
      |. Parser.keyword "(..)"
  )
  , Parser.map Subset (
      Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Helpers.whitespace
        , item = exposureName
        , trailing = Parser.Forbidden
        }
      |> Parser.map (
        List.foldl
          (\name res -> case name of
            ExposedFunction n -> { res | functions = n :: res.functions }
            ExposedOperator n -> { res | operators = n :: res.operators }
            ExposedType n -> { res | types = n :: res.types }
          )
          { functions = [], types = [], operators = [] }
      )
  )
  ]

type ExposureName
  = ExposedFunction String
  | ExposedOperator String
  | ExposedType String

exposureName: Parser ExposureName
exposureName =
  Parser.oneOf
  [ Helpers.functionName
    |> Parser.map ExposedFunction
  , Parser.succeed identity
    |. Parser.symbol "("
    |= Helpers.operatorName
    |. Parser.symbol ")"
    |> Parser.map ExposedOperator
  , subTypesName
    |> Parser.map ExposedType
  ]

subTypesName: Parser String
subTypesName =
  Helpers.typeName
  |> Parser.andThen (\name ->
    Parser.oneOf
      [ Parser.keyword "(..)"
        |> Parser.source
      , Parser.sequence
          { start = "("
          , separator = ","
          , end = ")"
          , spaces = Helpers.spaces
          , item = Parser.lazy (\_ -> subTypesName)
          , trailing = Parser.Forbidden
          }
        |> Parser.source
      , Parser.succeed ""
      ]
    |> Parser.map (\sub -> name ++ sub)
  )

-- JSON

encode: Exposure -> Encode.Value
encode exp =
  case exp of
    All -> Encode.string "*"
    Subset { functions, types, operators } ->
      Encode.object
      [ ("functions", Helpers.encodeList Encode.string functions)
      , ("types", Helpers.encodeList Encode.string types)
      , ("operators", Helpers.encodeList Encode.string operators)
      ]
