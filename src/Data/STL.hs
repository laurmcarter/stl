{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.STL where

import Control.Applicative
import Control.Lens
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import Data.Serialize
import Data.Foldable (Foldable(..))
import qualified Data.Set as Set
-- import qualified Data.Sequence as S
import Data.Word
import Linear

readSTLFile :: FilePath -> IO (Either String STL)
readSTLFile fp = do
  bs <- BS.readFile fp
  return $ decodeSTL bs

decodeSTL :: BS.ByteString -> Either String STL
decodeSTL = decode

-- Commented {{{

data Commented a = Commented
  { comment :: BS.ByteString
  , commented :: a
  } deriving (Eq,Ord,Show)

instance (Serialize a) => Serialize (Commented a) where
 put c = do
   put $ BS.take 80 $ comment c
   put $ commented c
 get = Commented <$> getBytes 80 <*> get

-- }}}

-- Solid {{{

data Solid a = Solid
  { solidName :: Maybe BS.ByteString
  , solidFacets :: Set.Set (Facet a)
  } deriving (Eq,Ord,Show)

instance (Ord a, Serialize a) => Serialize (Solid a) where
  put s = do
    put sz
    put fs
    where
    fs = solidFacets s
    sz :: Word32
    sz = fromIntegral $ Set.size fs
  get = do
    void getFacetCount
    fs <- get
    return Solid
      { solidName = Nothing
      , solidFacets = fs
      }
    where
    getFacetCount :: Get Word32
    getFacetCount = get

instance (Ord a, Ascii a) => Ascii (Solid a) where
  fromAscii = do
    nm <- word_ "solid" <+> anyWord
    spaces1
    fs <- fromAscii `sepBy1` spaces1
    spaces1
    word_ "endsolid"
    spaces
    return Solid
      { solidName = Just nm
      , solidFacets = Set.fromList fs
      }

-- }}}

-- Facet {{{

data Facet a = Facet
  { facetNormal   :: Vertex a
  , facetVertices :: V3 (Vertex a)
  } deriving (Eq,Ord,Show)

instance (Serialize a) => Serialize (Facet a) where
  put f = do
    put $ facetNormal f
    put $ facetVertices f ^. from v3
    putAttrBytes 0
    where
    putAttrBytes :: Putter Word16
    putAttrBytes = put
  get = do
    n <- get
    v <- get
    void getAttrBytes
    return Facet
      { facetNormal = n
      , facetVertices = v ^. v3
      }
    where
    getAttrBytes :: Get Word16
    getAttrBytes = get

instance (Ascii a) => Ascii (Facet a) where
  fromAscii = do
    n <- word_ "facet" <+> word_ "normal" <+> fromAscii
    spaces1
    word_ "outer" <+> word_ "loop"
    spaces1
    v <- parseV3 $ word_ "vertex" <+> fromAscii
    spaces1
    word_ "endloop"
    spaces1
    word_ "endfacet"
    return Facet
      { facetNormal = n
      , facetVertices = v
      }

mkFacet :: Vertex a -> Vertex a -> Vertex a -> Vertex a -> Facet a
mkFacet n a b c = Facet n $ V3 a b c

-- }}}

-- Vertex {{{

newtype Vertex a = Vertex
  { vertex :: V3 a
  } deriving
    ( Eq, Ord, Show, Num, Fractional
    , R1, R2, R3, Additive
    , Functor, Applicative, Monad
    , Foldable, Traversable
    )

instance (Serialize a) => Serialize (Vertex a) where
  put = put . view (from v3) . vertex
  get = fmap (Vertex . view v3) get

instance (Ascii a) => Ascii (Vertex a) where
  fromAscii = do
    x <- fromAscii
    spaces1
    y <- fromAscii
    spaces1
    z <- fromAscii
    return $ mkVertex x y z

mkVertex :: a -> a -> a -> Vertex a
mkVertex a b c = Vertex $ V3 a b c

-- }}}

-- STLUnit {{{

newtype STLUnit = STLUnit
  { stlUnit :: Float
  } deriving (Eq,Ord,Show,Num,Fractional)

instance Serialize STLUnit where
  put = putFloat32le . stlUnit
  get = fmap STLUnit getFloat32le

instance Ascii STLUnit where
  fromAscii = fmap STLUnit float

-- }}}

type STL    = Commented Object
type Object = Solid  STLUnit
type Face   = Facet  STLUnit
type Vert   = Vertex STLUnit

v3 :: Iso (a,a,a) (b,b,b) (V3 a) (V3 b)
v3 = iso fromTriple toTriple
  where
  toTriple (V3 a b c) = (a,b,c)
  fromTriple (a,b,c) = V3 a b c

class Ascii a where
  fromAscii  :: Parser a
  -- render :: Render a

type Builder = Int -> BS.Builder
type Render a = a -> Builder

-- Parser {{{

spaces1 :: Parser ()
spaces1 = void $ takeWhile1 isSpace

spaces :: Parser ()
spaces = void $ A.takeWhile isSpace

float :: Parser Float
float = fmap fromReal double

anyWord :: Parser BS.ByteString
anyWord = takeWhile1 (not . isSpace)

fromReal :: (Real a, Fractional b) => a -> b
fromReal = fromRational . toRational

word_ :: BS.ByteString -> Parser ()
word_ = void . string

(<+>) :: Parser a -> Parser b -> Parser b
pa <+> pb = do
  void pa
  spaces1
  pb

infixr <+>

parseV3 :: Parser a -> Parser (V3 a)
parseV3 pa = V3 <$> pa <* spaces1 <*> pa <* spaces1 <*> pa

-- }}}

