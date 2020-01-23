{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}

-- | Type classes in this module are intended to be derived with DeriveGeneric.
module Text.PrettyPrint.Records (FQuery, VQuery, RFmt, TFmt, format, formatUntil
    , fields, values, dfltFmt, simpleFmt, tableFmt, Format(..), TableFmt(..)
    , formatTable) where

import GHC.Generics
import Data.Typeable (cast, Typeable)
import Text.PrettyPrint.Boxes ((<+>), vcat, text, Box(), left, hsep, top)
import Data.List (transpose)

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
    -- | See 'FQuery'.
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
    -- | See 'VQuery'.
    values :: a -> [String]
    default values :: (Generic a, VQuery' (Rep a)) => a -> [String]
    values = values' . from

-- RFmt ------------------------------------------------------------------------

-- |A Format is responsible for converting a Record into a formatted Box.
--
-- @
--    __Example:__
--        dfltFmt :: Format a
--        dfltFmt = Format
--            { arg = text
--            , label = \\a b -> text (a <> ":") <+> b
--            , finally = vcat left }
-- @
--
data Format a = Format
        { -- | Converts Record fields to Boxes
          arg     :: String -> Box,
          -- | Merge accessor with field
          label :: String -> Box -> Box,
          -- | Finally reduce list of boxes
          finally :: [Box]  -> Box
        }

class RFmt' f where
    fvalues' :: (Typeable a, RFmt a) => Int -> f p -> Format a -> [Box]

-- |Reduce Record to Box given a Format.
--
-- @
--
--   __Example:__
--       data List a = List { label :: String, val :: a, tail :: List a }
--           | Nil deriving (Generic, Show)
--       instance FQuery (List a)
--       instance (Show a, Typeable a) => RFmt (List a)
--       test = List "head" 3 $ List "mid" 4 $ List "tail" 5 $ Nil
--
--       ghci> printBox $ format test dfltFmt
--       label: "head"
--       val: 3
--       tail: label: "mid"
--             val: 4
--             tail: label: "tail"
--                   val: 5
--                   tail: Nil
-- @
--
class (Typeable a, FQuery a, Show a) => RFmt a where
    fvalues :: Int -> a -> Format a -> [Box]
    default fvalues :: (Generic a, RFmt' (Rep a)) => Int -> a -> Format a -> [Box]
    fvalues 0 _ _ = [text "....."]
    fvalues n a f = fvalues' n (from a) f

instance RFmt' f => RFmt' (M1 t c f) where
    fvalues' n (M1 x) = fvalues' n x

instance (RFmt' f, RFmt' g) => RFmt' (f :+: g) where
    fvalues' n (L1 x) = fvalues' n x
    fvalues' n (R1 x) = fvalues' n x

instance (RFmt' f, RFmt' g) => RFmt' (f :*: g) where
    fvalues' n (x :*: y) f = let
        l = fvalues' n x f
        r = fvalues' n y f
        in l <> r

instance (Typeable c, Show c) => RFmt' (Rec0 c) where
    fvalues' n K1{unK1=v} f = let
        g :: Typeable k => Format k -> Maybe k
        g _ = cast v
        in case g f of
            Nothing -> [arg f . show $ v]
            Just x  -> [formatUntil (n - 1) x f]

instance RFmt' U1 where
    fvalues' _ _ _ = []

instance RFmt' V1 where
    fvalues' _ _ _ = []

-- TFmt ------------------------------------------------------------------------

class TFmt' f where
    tvalues' :: f p -> [String]

instance TFmt' f => TFmt' (M1 t c f) where
    tvalues' (M1 x) = tvalues' x

instance (TFmt' f, TFmt' g) => TFmt' (f :*: g) where
    tvalues' (x :*: y) = tvalues' x <> tvalues' y

instance (Show c) => TFmt' (Rec0 c) where
    tvalues' K1{unK1=v} = [show v]

instance TFmt' U1 where
    tvalues' _ = []

instance TFmt' V1 where
    tvalues' _ = []

-- | Identical to 'VQuery' except no instance for sum types (a necessary
-- restriction for printing tables).
class TFmt a where
    tvalues :: a -> [String]
    default tvalues :: (Generic a, TFmt' (Rep a)) => a -> [String]
    tvalues = tvalues' . from


-- Helpers ---------------------------------------------------------------------

data TableFmt = TableFmt
    { -- | Converts Record fields to Boxes.
      argT     :: String -> Box,
      -- | Merge accessor with field.
      labelT :: String -> [Box] -> Box,
      -- | Finally reduce list of boxes.
      finallyT :: [Box]  -> Box
    }

dfltFmt :: Format a
dfltFmt = Format
    { arg = text
    , label = \a b -> text (a <> ":") <+> b
    , finally = vcat left }

-- | A table formatter which produces a top-down table.
tableFmt :: TableFmt
tableFmt = TableFmt
    { argT = text
    , labelT = \a bs -> vcat left $ text a : bs
    , finallyT = hsep 1 top }

-- | Given String representation of accessor and value return b.
simpleFmt :: (VQuery a, FQuery a) => (String -> String -> b) -> a  -> [b]
simpleFmt f x = zipWith f (fields x) (values x)

-- | Format Record a with up to n levels of recursivity.
formatUntil :: RFmt a => Int -> a -> Format a -> Box
formatUntil n a f = let
    lhs = fields a
    rhs = fvalues n a f
    in  if null rhs
        then arg f . show $ a
        else finally f $ zipWith (label f) lhs rhs

-- | Format Record a record fully expanding the data structure if it is
-- recursive.
format :: RFmt a => a -> Format a -> Box
format = formatUntil (-1)

-- | Format a list of (FQuery, TFmt) instances as a table.
--
-- @
--    __Example:__
--        data Employee = E
--           { eId :: Int
--           , name :: String
--           , email :: String } deriving (Generic, Show)
--        instance FQuery Employee
--        instance TFmt Employee
--        e1 = E 3 \"John\" "johnbelcher@foobar.xyz"
--        e1 = E 3 \"John\" "johnbelcher@foobar.xyz"
--        e2 = E 17 \"Maria\" "Mariafoobar@net.net"
--        e3 = E 1 \"Stanley\" "stanleytheceo@foobar.org"
--        e4 = E 2 \"Kayla\" "klabar@foo.com"
--        e5 = E 4 \"Sammy\" "sammersfoobar@foo.bar"
--
--        ghci> printBox $ formatTable [e1, e2, e3, e4, e5] tableFmt
--        eId name      email
--        3   \"John\"    "johnbelcher@foobar.xyz"
--        17  \"Maria\"   "Mariafoobar@net.net"
--        1   \"Stanley\" "stanleytheceo@foobar.org"
--        2   \"Kayla\"   "klabar@foo.com"
--        4   \"Sammy\"   "sammersfoobar@foo.bar"
-- @
--
formatTable :: (TFmt a, FQuery a) => [a] -> TableFmt -> Box
formatTable as t = let
    header = fields (head as)
    fvals  = transpose $ fmap (argT t) . tvalues <$> as
    in finallyT t $ zipWith (labelT t) header fvals
