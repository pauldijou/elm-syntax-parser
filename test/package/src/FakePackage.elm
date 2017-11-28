module FakePackage exposing (..)

import Json.Decode as Decode exposing (Decoder)

type alias Alias = Int

type alias Record =
  { f1: String
  , f2: Int
  }

type alias Record2 a =
  {
  a
  |
  f1
  :
  Int
  }

fortyTwo:
  Int
-- Whatever
  ->
  Bool
  ->  -- Something
  String
fortyTwo
-- Just for fun
  i  -- aze
  {- Hey
    {- Nested -}
    -- Inline for fun
-- No indentation
  Woot
  -}
  b  -- aze
  =
  "42"

-- type alias Record =
--   { field1: String
--   , field2: Alias
--   , field3: { f1: Bool, f2: Maybe Float }
--   }

type Option a = None | Some a

type Toto a b
  = Tata a
  | Titi Int b
  | Tutu

something: Toto a b -> Alias -> Option b
something toto als =
  None

tuple: a -> b -> (a, b)
tuple =
  (,)
