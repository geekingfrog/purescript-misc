module Main where

import Prelude
import Effect                   (Effect)
import Effect.Console           (log)
import Data.Newtype             (class Newtype, unwrap)
import Data.Generic.Rep         as G
import Data.Nullable            as N
import Data.Lens                ((^.))
import Data.Lens                as Lens
import Data.Lens.Record         (prop)
import Data.Lens.Prism.Maybe    (_Just)
import Data.Lens.Iso.Newtype    (_Newtype)
import Data.Maybe               as Mb
import Data.Symbol              (SProxy(..))


newtype Outer
  = Outer
  { inner                :: N.Nullable Inner
  , irrelevantOuterStuff :: String
  }

derive instance newtypeOuter :: Newtype Outer _
derive instance genericOuter :: G.Generic Outer _

newtype Inner
  = Inner
  { id    :: InnerId
  , foo   :: Int
  }

derive instance newtypeInner :: Newtype Inner _
derive instance genericInner :: G.Generic Inner _

newtype InnerId = InnerId String

derive instance newtypeInnerId :: Newtype InnerId _
derive instance genericInnerId :: G.Generic InnerId _

someOuter :: Outer
someOuter = Outer
  { irrelevantOuterStuff: "irrelevantOuter"
  , inner: N.toNullable $ Mb.Just $ Inner
    { id: InnerId "innerId1"
    , foo: 2
    }
  }


-- this works, but `Lens.to unwrap` feels sketchy
getter1 :: Outer -> String
getter1 o = o
  ^. Lens.to unwrap
  <<< prop (SProxy :: SProxy "inner")
  <<< Lens.to N.toMaybe
  <<< _Just
  <<< Lens.to unwrap
  <<< prop (SProxy :: SProxy "id")
  <<< Lens.to unwrap


-- | this doesn't compile. error is:
-- No type class instance was found for
--
--   Data.Newtype.Newtype t3
--                        { inner :: t4
--                        , irrelevantOuterStuff :: String
--                        }
--

-- getter2 :: Outer -> String
-- getter2 o = o
--   ^. _Newtype -- replacing the `Lens.to unwrap` with _Newtype
--   <<< prop (SProxy :: SProxy "inner")
--   <<< Lens.to N.toMaybe
--   <<< _Just
--   <<< Lens.to unwrap
--   <<< prop (SProxy :: SProxy "id")
--   <<< Lens.to unwrap


main :: Effect Unit
main = do
  let innerId = getter1 someOuter

  log innerId
