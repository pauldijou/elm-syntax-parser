module SyntaxParser exposing (..)

import Task exposing (Task)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Parser exposing (Parser, (|.), (|=))
import Elm.Docs as Docs

import Node.FileSystem as FS
import Node.Stats as Stats
import Node.Path as Path
import Node.Constants exposing (Encoding(..))

import Statement
import Package exposing (Package)
import Module exposing (Module)


init: String -> Task String Package
init elmPackagePath =
  FS.readFile elmPackagePath { encoding = Utf8, flag = "" }
  |> Task.mapError .message
  |> Task.andThen (\elmJson ->
    case Decode.decodeString Package.decoderJson elmJson of
      Err err -> Task.fail err
      Ok json -> Task.succeed (Package.fromJson elmPackagePath json)
  )


parse: String -> Task String Package
parse path =
  init path
  |> Task.andThen (\package ->
    List.foldl
      (\source res ->
        res
        |> Task.andThen (\pkg ->
          parseSource (Path.resolve3 pkg.root ".." source)
          |> Task.map (\modules ->
            { pkg | modules = Dict.union pkg.modules modules }
          )
        )
      )
      (Task.succeed package)
      package.sources
  )


parseSource: String -> Task String (Dict String Module)
parseSource path =
  parseDirectory path
  |> Task.map (\modules ->
    modules
    |> List.map (\m -> (m.name, m))
    |> Dict.fromList
  )


parseDirectory: String -> Task String (List Module)
parseDirectory path =
  FS.readdir path Utf8
  |> Task.mapError .message
  |> Task.andThen (
    List.foldl
      (\p res ->
        let
          fullPath = Path.resolve2 path p
        in
          Task.map2 (,) res (FS.stat fullPath |> Task.mapError .message)
          |> Task.andThen (\(modules, stats) ->
            if (Stats.isFile stats)
            then parseFile fullPath |> Task.map (\m -> m :: modules)
            else if (Stats.isDirectory stats)
            then parseDirectory fullPath |> Task.map (\m -> List.append m modules)
            else Task.succeed []
          )
      )
      (Task.succeed [])
  )


parseFile: String -> Task String Module
parseFile path =
  FS.readFile path { encoding = Utf8, flag = "" }
  |> Task.mapError .message
  |> Task.andThen (\content ->
    if content == ""
    then Task.fail ("Empty file " ++ path)
    else Task.succeed content
  )
  |> Task.map Statement.fromSource
  |> Task.andThen (\strStmts ->
    case strStmts of
      [] ->
        Task.fail ("No statements found in " ++ path)
      first :: rest ->
        case Parser.run Module.parserDeclaration first of
          Err err ->
            Task.fail ("Failed to parse module declaration: " ++ (toString err))
          Ok decl ->
            List.foldl
              reduceStatement
              { imports = []
              , aliases = []
              , unions = []
              , values = []
              }
              rest
            |> Task.succeed
            |> Task.map (\stmts -> (decl, stmts))
  )
  |> Task.map (\(declaration, stmts) ->
    { name = declaration.name
    , path = path
    , imports = List.reverse stmts.imports
    , exposure = declaration.exposure
    , ports = []
    , effects = Module.NoEffect
    , aliases = List.reverse stmts.aliases
    , unions = List.reverse stmts.unions
    , values = List.reverse stmts.values
    }
  )

type alias ParseContext =
  { imports: List Module.Import
  , aliases: List Docs.Alias
  , unions: List Docs.Union
  , values: List Docs.Value
  }

reduceStatement: String -> ParseContext -> ParseContext
reduceStatement strStmt ctx =
  case Parser.run Statement.parser strStmt of
    Err _ -> ctx
    Ok stmt -> case stmt of
      Statement.Import imp ->
        { ctx | imports = imp :: ctx.imports }
      Statement.Alias name args tipe ->
        { ctx | aliases = { name = name, comment = "", args = args, tipe = tipe } :: ctx.aliases }
      Statement.Union name args tags ->
        { ctx | unions = { name = name, comment = "", args = args, tags = tags } :: ctx.unions }
      Statement.Value name tipe ->
        { ctx | values = { name = Docs.Name name, comment = "", tipe = tipe } :: ctx.values }
