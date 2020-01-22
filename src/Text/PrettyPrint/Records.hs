{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

module Text.PrettyPrint.Records (FQuery, VQuery, VFmt, format, fields, values,
    dfltFmt, simpleFmt, Formatter(..)) where

import GHC.Generics
import Data.Data
import Text.PrettyPrint.Boxes hiding ((<>))

--- FQuery -------------------------------------------------------------------

class FQuery' f where
    fields' :: f p -> [String]

instance FQuery' V1 where
    fields' _ = []

instance FQuery' U1 where
    fields' _ = []

instance FQuery c => FQuery' (Rec0 c) where
    fields' _ = []

instance (FQuery' f, FQuery' g) => FQuery' (f :+: g) where
    fields' (L1 x) = fields' x
    fields' (R1 x) = fields' x

instance (FQuery' f, FQuery' g) => FQuery' (f :*: g) where
    fields' (x :*: y) = fields' x <> fields' y

instance FQuery' f => FQuery' (D1 c f) where
    fields' (M1 x) = fields' x

instance FQuery' f => FQuery' (C1 c f) where
    fields' (M1 x) = fields' x

instance Selector s => FQuery' (S1 s f) where
    fields' x = [selName x]

-- | Accumulate Record accessors as [String] (in order of definition).
class FQuery a where
    fields :: a -> [String]
    default fields :: (Generic a, FQuery' (Rep a)) => a -> [String]
    fields = fields' . from

-- VQuery ----------------------------------------------------------------------

class VQuery' f where
    values' :: f p -> [String]

instance VQuery' f => VQuery' (M1 t c f) where
    values' (M1 x) = values' x

instance (VQuery' f, VQuery' g) => VQuery' (f :+: g) where
    values' (L1 x) = values' x
    values' (R1 x) = values' x

instance (VQuery' f, VQuery' g) => VQuery' (f :*: g) where
    values' (x :*: y) = values' x <> values' y

instance (Show c) => VQuery' (Rec0 c) where
    values' K1{unK1=v} = [show v]

instance VQuery' U1 where
    values' _ = []

instance VQuery' V1 where
    values' _ = []

-- | Accumulate Record values as [String] (in order of definition).
class VQuery a where
    values :: a -> [String]
    default values :: (Generic a, VQuery' (Rep a)) => a -> [String]
    values = values' . from

-- VFmt ------------------------------------------------------------------------

-- |A Formatter is responsible for converting a Record into a formatted Box.
--
-- @
--    __Example:__
--        dfltFmt :: Formatter a
--        dfltFmt = Formatter
--            { arg = text
--            , label = \\a b -> text (a <> ":") <+> b
--            , finally = vcat left }
-- @
--
data Formatter a = Formatter
        { -- | Converts Record fields to Boxes
          arg     :: String -> Box,
          -- | Merge accessor with field
          label :: String -> Box -> Box,
          -- | Finally reduce list of boxes
          finally :: [Box]  -> Box
        }

class VFmt' f where
    fvalues' :: (Typeable a, VFmt a) => f p -> Formatter a -> [Box]

-- |Reduce Record to Box given a Formatter
--
-- @
--
--   __Example:__
--      data List a = List { label :: String, val :: a, tail :: List a }
--          | Nil deriving (Generic, Show)
--      instance FQuery (List a)
--      instance (Show a, Typeable a) => VFmt (List a)
--      test = List "head" 3 $ List "mid" 4 $ List "tail" 5 $ Nil
--
--      ghci> printBox $ format test dfltFmt
--      label: "head"
--      val: 3
--      tail: label: "mid"
--            val: 4
--            tail: label: "tail"
--                  val: 5
--                  tail: Nil
-- @
--
class (Typeable a, FQuery a, Show a) => VFmt a where
    fvalues :: a -> Formatter a -> [Box]
    default fvalues :: (Generic a, VFmt' (Rep a)) => a -> Formatter a -> [Box]
    fvalues a = fvalues' (from a)
    format :: a -> Formatter a -> Box
    format a f = let
        lhs = fields a
        rhs = fvalues a f
        in  if null rhs
            then arg f . show $ a
            else finally f $ zipWith (label f) lhs rhs

instance VFmt' f => VFmt' (M1 t c f) where
    fvalues' (M1 x) = fvalues' x

instance (VFmt' f, VFmt' g) => VFmt' (f :+: g) where
    fvalues' (L1 x) = fvalues' x
    fvalues' (R1 x) = fvalues' x

instance (VFmt' f, VFmt' g) => VFmt' (f :*: g) where
    fvalues' (x :*: y) f = let
        l = fvalues' x f
        r = fvalues' y f
        in l <> r

instance (Typeable c, Show c) => VFmt' (Rec0 c) where
    fvalues' K1{unK1=v} f = let
        g :: Typeable k => Formatter k -> Maybe k
        g _ = cast v
        in case g f of
            Nothing -> [arg f . show $ v]
            Just x  -> [format x f]

instance VFmt' U1 where
    fvalues' _ _ = []

instance VFmt' V1 where
    fvalues' _ _ = []

-- Helpers ---------------------------------------------------------------------

dfltFmt :: Formatter a
dfltFmt = Formatter
    { arg = text
    , label = \a b -> text (a <> ":") <+> b
    , finally = vcat left }

-- | Given String representation of accessor and value return b
simpleFmt :: (VQuery a, FQuery a) => (String -> String -> b) -> a  -> [b]
simpleFmt f x = zipWith f (fields x) (values x)
