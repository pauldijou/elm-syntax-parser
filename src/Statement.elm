module Statement exposing (..)

import Parser exposing (Parser, (|.), (|=))
import Elm.Docs.Type as Docs

import Helpers
import Module
import Type exposing (Type)

type Statement
  = Import Module.Import
  | Alias String (List String) Type
  | Union String (List String) (List (String, List Type))
  | Value String Type


parser: Parser Statement
parser =
  Parser.oneOf
    [ Parser.map Import Module.parserImport
    , parserAlias
    , parserUnion
    , parserValue
    ]

grabSeveral: Parser () -> Parser a -> List a -> Parser (List a)
grabSeveral separator parser args =
  Parser.lazy (\_ ->
    Parser.oneOf
      -- From the 2nd item
      [ Parser.delayedCommit separator parser
        |> Parser.andThen (\arg -> grabSeveral separator parser (arg :: args))
      -- The 1st item
      , parser
        |> Parser.andThen (\arg -> grabSeveral separator parser (arg :: args))
      -- Returning result when all items parsed
      , Parser.succeed (List.reverse args)
      ]
  )

parserArgs: Parser (List String)
parserArgs =
  grabSeveral Helpers.spaces Helpers.functionName []

parserAlias: Parser Statement
parserAlias =
  Parser.delayedCommit
    (
      Parser.succeed identity
      |. Parser.keyword "type"
      |. Helpers.spaces
      |. Parser.keyword "alias"
    )
    (
      Parser.succeed Alias
      |. Helpers.spaces
      |= Helpers.typeName
      |. Helpers.spaces
      |= parserArgs
      |. Helpers.whitespace
      |. Parser.symbol "="
      |. Helpers.whitespace
      |= Type.parser
    )

parserUnionSeparator: Parser ()
parserUnionSeparator =
  Parser.succeed ()
  |. Helpers.whitespace
  |. Parser.symbol "|"
  |. Helpers.spaces

parserUnionTag: Parser (String, List Type)
parserUnionTag =
  Type.parser
  |> Parser.andThen (\tipe -> case tipe of
    Docs.Type name tags -> Parser.succeed (name, tags)
    _ -> Parser.fail ("An union type should starts with a Type: " ++ (toString tipe))
  )

parserUnion: Parser Statement
parserUnion =
  Parser.delayedCommitMap
    (\name (args, tags) -> Union name args tags)
    (
      Parser.succeed identity
      |. Parser.keyword "type"
      |. Helpers.spaces
      |= Helpers.typeName
    )
    (
      Parser.succeed (,)
      |. Helpers.spaces
      |= parserArgs
      |. Helpers.whitespace
      |. Parser.symbol "="
      |. Helpers.whitespace
      |= grabSeveral parserUnionSeparator parserUnionTag []
    )


parserValue: Parser Statement
parserValue =
  Parser.delayedCommitMap
    (\name tipe -> Value name tipe)
    (
      Parser.succeed identity
      |= Helpers.functionName
      |. Helpers.spaces
      |. Parser.symbol ":"
    )
    (
      Parser.succeed identity
      |. Helpers.whitespace
      |= Type.parser
    )


type alias StatementContext = { cursor: String, comments: Int, statements: List String }

countComments: String -> StatementContext -> StatementContext
countComments line context =
  let
    openings = String.indexes "{-" line |> List.length
    closings = String.indexes "-}" line |> List.length
  in
    { context | comments = max 0 (context.comments + openings - closings) }

fromSource: String -> List String
fromSource content =
  content
  |> String.split "\n"
  |> (fromSourceRecursive { cursor = "", comments = 0, statements = [] })
  |> (\ctx ->
    if ctx.cursor == ""
    then ctx.statements
    else case ctx.statements of
      [] -> [ ctx.cursor ]
      last :: rest ->
        if String.startsWith " " ctx.cursor
        then (last ++ "\n" ++ ctx.cursor) :: rest
        else ctx.cursor :: last :: rest
  )
  |> List.reverse

fromSourceRecursive: StatementContext -> List String -> StatementContext
fromSourceRecursive context lines =
  case lines of
    [] -> context
    line :: rest ->
      -- we are inside a multilines comments,
      -- we need to keep track of when we exit it
      if context.comments > 0
      then fromSourceRecursive (countComments line context) rest
      -- ignore empty lines
      else if line == "" || line == "\n"
      then fromSourceRecursive context rest
      -- ignore one line comments
      else if String.startsWith "--" line
      then fromSourceRecursive context rest
      -- entering a multilines comments
      else if String.startsWith "{-" line
      then fromSourceRecursive (countComments line context) rest
      -- if there is "padding", it means we are still inside the same statement
      else if String.startsWith " " line
      then fromSourceRecursive { context | cursor = context.cursor ++ "\n" ++ line } rest
      -- we exited the statement but we still need to parse the current line
      else
        fromSourceRecursive
          { context
          | cursor = line
          , statements =
              if context.cursor == ""
              then context.statements
              else context.cursor :: context.statements
          }
          rest
