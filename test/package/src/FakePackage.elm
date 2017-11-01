module FakePackage exposing (..)

import Json.Decode as Decode exposing (Decoder)

type alias Alias = Int

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
