module Helpers exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Char
import Json.Encode as Encode
import Parser exposing (Parser, (|.), (|=))
import Parser.LanguageKit as Parser

encodeList: (a -> Encode.Value) -> List a -> Encode.Value
encodeList encode list =
  Encode.list (List.map encode list)

encodeDict: (a -> Encode.Value) -> Dict String a -> Encode.Value
encodeDict encode dict =
  dict
  |> Dict.toList
  |> List.map (\(key, value) -> (key, encode value))
  |> Encode.object

encodeMaybe: (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encode maybe =
  maybe
  |> Maybe.map encode
  |> Maybe.withDefault Encode.null

keywords: Set String
keywords =
  Set.fromList [ "let", "in", "case", "of", "port", "effect", "module", "exposing" ]

spaces : Parser ()
spaces =
  Parser.ignore Parser.zeroOrMore (\char -> char == ' ')

whitespaceChars: List Char
whitespaceChars = [ ' ', '\n', '\r', '\t' ]

whitespace: Parser ()
whitespace =
  Parser.ignore Parser.zeroOrMore (\c -> List.member c whitespaceChars)

anyName: Parser String
anyName = Parser.variable isVarChar isVarChar keywords

functionName: Parser String
functionName = Parser.variable Char.isLower isVarChar keywords

typeName: Parser String
typeName = Parser.variable Char.isUpper isVarChar keywords

qualifiedName: Parser String
qualifiedName = Parser.variable Char.isUpper isQualifiedName keywords

operatorName: Parser String
operatorName = Parser.variable isOperatorName isOperatorName keywords

isVarChar: Char -> Bool
isVarChar char =
  Char.isLower char
  || Char.isUpper char
  || Char.isDigit char
  || char == '_'

isQualifiedName: Char -> Bool
isQualifiedName char =
  isVarChar char || char == '.'

isOperatorName: Char -> Bool
isOperatorName char =
  char /= '(' && char /= ')'
