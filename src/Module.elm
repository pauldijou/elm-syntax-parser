module Module exposing (..)

import Json.Encode as Encode
import Elm.Docs as Docs
import Parser exposing (Parser, (|.), (|=))

import Helpers
import Exposure exposing (Exposure)
import Type exposing (Type)

type alias Import =
  { name: String
  , alias: Maybe String
  , exposure: Maybe Exposure
  }

type alias Port = { name: String }

type Effect = NoEffect | CmdOnly String | SubOnly String | Both String String

type alias Module =
  { name: String
  , path: String
  , imports: List Import
  , exposure: Exposure
  , ports: List Port
  , effects: Effect
  , aliases: List Docs.Alias
  , unions: List Docs.Union
  , values: List Docs.Value
  }


-- PARSER

type alias Declaration = { name: String, exposure: Exposure }

parserDeclaration: Parser Declaration
parserDeclaration =
  Parser.inContext "module declaration" <|
  Parser.succeed Declaration
  |. Parser.keyword "module"
  |. Helpers.spaces
  |= Helpers.qualifiedName
  |. Helpers.spaces
  |. Parser.keyword "exposing"
  |. Helpers.whitespace
  |= Exposure.parser
  |. Parser.end

parserImportAlias: Parser String
parserImportAlias =
  Parser.succeed identity
  |. Parser.keyword "as"
  |. Helpers.spaces
  |= Helpers.typeName

parserImport: Parser Import
parserImport =
  Parser.inContext "import" <|
  Parser.succeed Import
  |. Parser.keyword "import"
  |. Helpers.spaces
  |= Helpers.qualifiedName
  |. Helpers.spaces
  |= Parser.oneOf
    [ Parser.map Just parserImportAlias
    , Parser.succeed Nothing
    ]
  |. Helpers.spaces
  |= Parser.oneOf
    [ Parser.map Just <|
      Parser.succeed identity
      |. Parser.keyword "exposing"
      |. Helpers.spaces
      |= Exposure.parser
    , Parser.succeed Nothing
    ]
  |. Parser.end

-- JSON

encode: Module -> Encode.Value
encode m =
  Encode.object
  [ ("name", Encode.string m.name)
  , ("path", Encode.string m.path)
  , ("imports", Helpers.encodeList encodeImport m.imports)
  , ("exposure", Exposure.encode m.exposure)
  , ("ports", Helpers.encodeList encodePort m.ports)
  , ("effects", encodeEffect m.effects)
  , ("aliases", Helpers.encodeList encodeAlias m.aliases)
  , ("unions", Helpers.encodeList encodeUnion m.unions)
  , ("values", Helpers.encodeList encodeValue m.values)
  ]


encodeEffect: Effect -> Encode.Value
encodeEffect eff =
  case eff of
    NoEffect -> Encode.object []
    CmdOnly n -> Encode.object [ ("cmd", Encode.string n) ]
    SubOnly n -> Encode.object [ ("sub", Encode.string n) ]
    Both c s -> Encode.object [ ("cmd", Encode.string c), ("sub", Encode.string s) ]


encodeImport: Import -> Encode.Value
encodeImport imp =
  Encode.object
  [ ("name", Encode.string imp.name)
  , ("alias", Helpers.encodeMaybe Encode.string imp.alias)
  , ("exposure", Helpers.encodeMaybe Exposure.encode imp.exposure)
  ]

encodePort: Port -> Encode.Value
encodePort p =
  Encode.object
  [ ("name", Encode.string p.name)
  ]

encodeAlias: Docs.Alias -> Encode.Value
encodeAlias a =
  Encode.object
  [ ("name", Encode.string a.name)
  , ("comment", Encode.string a.comment)
  , ("args", Helpers.encodeList Encode.string a.args)
  , ("tipe", Type.encode a.tipe)
  ]

encodeUnionTag: (String, List Type) -> Encode.Value
encodeUnionTag (name, types) =
  Encode.object
  [ ("name", Encode.string name)
  , ("args", Helpers.encodeList Type.encode types)
  ]

encodeUnion: Docs.Union -> Encode.Value
encodeUnion u =
  Encode.object
  [ ("name", Encode.string u.name)
  , ("comment", Encode.string u.comment)
  , ("args", Helpers.encodeList Encode.string u.args)
  , ("tags", Helpers.encodeList encodeUnionTag u.tags)
  ]

encodeValueAssociativity: Docs.Associativity -> Encode.Value
encodeValueAssociativity assoc =
  case assoc of
    Docs.Left  -> Encode.string "left"
    Docs.None  -> Encode.string "none"
    Docs.Right -> Encode.string "right"

encodeValueName: Docs.Name -> List (String, Encode.Value)
encodeValueName name =
  case name of
    Docs.Name str ->
      [ ("name", Encode.string str)
      , ("isOperator", Encode.bool False)
      ]
    Docs.Op str assoc precedence ->
      [ ("name", Encode.string str)
      , ("isOperator", Encode.bool True)
      , ("associativity", encodeValueAssociativity assoc)
      , ("precedence", Encode.int precedence)
      ]

encodeValue: Docs.Value -> Encode.Value
encodeValue v =
  [ encodeValueName v.name
  , [ ("comment", Encode.string v.comment)
    , ("tipe", Type.encode v.tipe)
    ]
  ]
  |> List.concat
  |> Encode.object
