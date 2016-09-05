{-# LANGUAGE RecordWildCards #-}

module Graphics.HVIF where

import           Control.Monad (replicateM, when)
import           Data.Bits ((.|.), clearBit, shift, testBit)
import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Serialize
import           Data.Word

data HVIFFile = HVIFFile
  { hvifColors :: Seq Style
  , hvifPaths  :: Seq Path
  , hvifShapes :: Seq Shape
  } deriving (Eq, Show)

decodeFile :: ByteString -> Either String HVIFFile
decodeFile = runGet pFile

getSeveral :: Get a -> Get (Seq a)
getSeveral getter = do
  count <- getWord8
  S.fromList `fmap` replicateM (fromIntegral count) getter

pFile :: Get HVIFFile
pFile = do
  header <- getByteString 4
  when (header /= "ncif") $
    fail "Missing `ficn' header"
  hvifColors <- getSeveral pStyle
  hvifPaths  <- getSeveral pPath
  hvifShapes <- getSeveral pShape
  return HVIFFile { .. }

-- Style Section

data Style
  = ColorSolid Word8 Word8 Word8 Word8
  | ColorGradient Gradient
  | ColorSolidNoAlpha Word8 Word8 Word8
  | ColorSolidGray Word8 Word8
  | ColorSolidGrayNoAlpha Word8
    deriving (Eq, Show)

pStyle :: Get Style
pStyle = do
  sType <- getWord8
  case sType of
    0x01 -> ColorSolid <$> get <*> get <*> get <*> get
    0x02 -> ColorGradient <$> pGradient
    0x03 -> ColorSolidNoAlpha <$> get <*> get <*> get
    0x04 -> ColorSolidGray <$> get <*> get
    0x05 -> ColorSolidGrayNoAlpha <$> get
    _    -> fail "invalid"


data Gradient = Gradient
  { gType  :: GradientType
  , gFlags :: GradientFlags
  , gStops :: Seq GradientStop
  } deriving (Eq, Show)

pGradient :: Get Gradient
pGradient = do
  gType  <- pGradientType
  gFlags <- pGradientFlags
  gStops <- getSeveral (pGradientStop gFlags)
  return Gradient { .. }

data GradientType
  = GTLinear
  | GTCircular
  | GTDiamond
  | GTConic
  | GTXY
  | GTSqrtXY
    deriving (Eq, Show)

pGradientType :: Get GradientType
pGradientType = do
  gType <- getWord8
  case gType of
    00 -> return GTLinear
    01 -> return GTCircular
    02 -> return GTDiamond
    03 -> return GTConic
    04 -> return GTXY
    05 -> return GTSqrtXY
    _  -> fail ("Unknown gradient type: " ++ show gType)


data GradientFlags = GradientFlags
  { gfTransform :: Bool
  , gfNoAlpha   :: Bool
  , gf16Bit     :: Bool
  , gfGrays     :: Bool
  } deriving (Eq, Show)

pGradientFlags :: Get GradientFlags
pGradientFlags = do
  gFlags <- getWord8
  return $ GradientFlags
    { gfTransform = testBit gFlags 1
    , gfNoAlpha   = testBit gFlags 2
    , gf16Bit     = testBit gFlags 3
    , gfGrays     = testBit gFlags 4
    }

data GradientStop = GradientStop
  { gsOffset :: Word8
  , gsRed    :: Word8
  , gsGreen  :: Word8
  , gsBlue   :: Word8
  , gsAlpha  :: Word8
  } deriving (Eq, Show)

pGradientStop :: GradientFlags -> Get GradientStop
pGradientStop flags = do
  offset <- get
  (r, g, b) <-
    if gfGrays flags
      then do
        val <- get
        return (val, val, val)
      else do
        r <- get
        g <- get
        b <- get
        return (r, g, b)
  a <-
    if gfNoAlpha flags
      then return 0xff
      else get
  return $ GradientStop offset r g b a


-- Path Section

data Path = Path
  { pathFlags  :: PathFlags
  , pathPoints :: Seq Point
  } deriving (Eq, Show)

pPath :: Get Path
pPath = do
  pathFlags <- pPathFlags
  pathPoints <- pPoints pathFlags
  return Path { .. }

data PathFlags = PathFlags
  { pfClosed       :: Bool
  , pfUsesCommands :: Bool
  , pfNoCurves     :: Bool
  } deriving (Eq, Show)

pPathFlags :: Get PathFlags
pPathFlags = do
  pFlags <- getWord8
  return $ PathFlags
    { pfClosed       = testBit pFlags 1
    , pfUsesCommands = testBit pFlags 2
    , pfNoCurves     = testBit pFlags 3
    }

data Point = Point
  { coordX :: Float
  , coordY :: Float
  } deriving (Eq, Show)

pPoints :: PathFlags -> Get (Seq Point)
pPoints PathFlags { pfUsesCommands = False } =
  getSeveral (Point <$> pCoord <*> pCoord)
pPoints PathFlags { pfUsesCommands = True } = do
  c1 <- getWord8
  c2 <- getWord8
  return $ S.empty

pCoord :: Get Float
pCoord = do
  b1 <- getWord8
  if testBit b1 7 then do
    b2 <- getWord8
    let cVal :: Word16 = (clearBit 7 (fromIntegral b1) `shift` 8) .|. fromIntegral b2
    return ((fromIntegral cVal / 102.0) - 128.0)
  else
    return (fromIntegral b1 - 32.0)

-- Shape Section

data Shape = Shape
  { shapeStyle :: StyleRef
  , shapePaths :: Seq PathRef
  , shapeFlags :: ShapeFlags
  } deriving (Eq, Show)

pShape :: Get Shape
pShape = do
  shapeStyle <- StyleRef <$> get
  shapePaths <- getSeveral (PathRef <$> get)
  shapeFlags <- pShapeFlags
  return Shape { .. }

newtype PathRef = PathRef { prIdx :: Word8 } deriving (Eq, Show)
newtype StyleRef = StyleRef { stIdx :: Word8 } deriving (Eq, Show)

data ShapeFlags = ShapeFlags
  { sfTransform       :: Bool
  , sfHinting         :: Bool
  , sfLodScale        :: Bool
  , sfHasTransformers :: Bool
  , sfTranslation     :: Bool
  } deriving (Eq, Show)

pShapeFlags :: Get ShapeFlags
pShapeFlags = do
  sFlags <- getWord8
  return ShapeFlags
    { sfTransform       = testBit sFlags 1
    , sfHinting         = testBit sFlags 2
    , sfLodScale        = testBit sFlags 3
    , sfHasTransformers = testBit sFlags 4
    , sfTranslation     = testBit sFlags 5
    }
