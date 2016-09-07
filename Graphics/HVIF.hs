{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MultiWayIf #-}

module Graphics.HVIF
(
-- * Decoding
  decodeFile
-- * Types
, HVIFFile(..)
-- ** Style Section
, Style(..)
, Gradient(..)
, GradientType(..)
, GradientFlags(..)
, GradientStop(..)
-- ** Paths Section
, Path(..)
, PathFlags(..)
, Point(..)
, Command(..)
-- ** Shape Section
, Shape(..)
, PathRef(..)
, StyleRef(..)
, ShapeFlags(..)
, Translation(..)
, LodScale(..)
) where

import           Control.Monad (replicateM, when)
import           Data.Bits ((.|.), (.&.), clearBit, shift, testBit)
import           Data.ByteString (ByteString)
import           Data.Sequence (Seq, (<|))
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

instance Serialize HVIFFile where
  get = pFile
  put = error "[unfinished]"

-- Style Section

data Style
  = ColorSolid Word8 Word8 Word8 Word8
  | ColorGradient Gradient
  | ColorSolidNoAlpha Word8 Word8 Word8
  | ColorSolidGray Word8 Word8
  | ColorSolidGrayNoAlpha Word8
    deriving (Eq, Show)

data Gradient = Gradient
  { gType  :: GradientType
  , gFlags :: GradientFlags
  , gStops :: Seq GradientStop
  } deriving (Eq, Show)

data GradientType
  = GTLinear
  | GTCircular
  | GTDiamond
  | GTConic
  | GTXY
  | GTSqrtXY
    deriving (Eq, Show)

data GradientFlags = GradientFlags
  { gfTransform :: Bool
  , gfNoAlpha   :: Bool
  , gf16Bit     :: Bool
  , gfGrays     :: Bool
  } deriving (Eq, Show)

data GradientStop = GradientStop
  { gsOffset :: Word8
  , gsRed    :: Word8
  , gsGreen  :: Word8
  , gsBlue   :: Word8
  , gsAlpha  :: Word8
  } deriving (Eq, Show)

-- Path Section

data Path = Path
  { pathFlags  :: PathFlags
  , pathPoints :: Seq Command
  } deriving (Eq, Show)

data PathFlags = PathFlags
  { pfClosed       :: Bool
  , pfUsesCommands :: Bool
  , pfNoCurves     :: Bool
  } deriving (Eq, Show)

data Point = Point
  { coordX :: Float
  , coordY :: Float
  } deriving (Eq, Show)

data Command
  = CmdHLine Float
  | CmdVLine Float
  | CmdLine Point
  | CmdCurve Point Point Point
    deriving (Eq, Show)

-- Shape Section

data Shape = Shape
  { shapeStyle     :: StyleRef
  , shapePaths     :: Seq PathRef
  , shapeFlags     :: ShapeFlags
  , shapeTransform :: Maybe Matrix
  , shapeTranslate :: Maybe Translation
  , shapeLodScale  :: Maybe LodScale
  , shapeTransList :: Seq Transformer
  } deriving (Eq, Show)

type Matrix = Seq Float

newtype PathRef = PathRef { prIdx :: Int } deriving (Eq, Show)
newtype StyleRef = StyleRef { stIdx :: Int } deriving (Eq, Show)

data ShapeFlags = ShapeFlags
  { sfTransform       :: Bool
  , sfHinting         :: Bool
  , sfLodScale        :: Bool
  , sfHasTransformers :: Bool
  , sfTranslation     :: Bool
  } deriving (Eq, Show)

data Translation = Translation
  { transX :: Float
  , transY :: Float
  } deriving (Eq, Show)

data LodScale = LodScale
  { lsMin :: Float
  , lsMax :: Float
  } deriving (Eq, Show)

data Transformer
  = TransformerAffine Matrix
  | TransformerContour Float Word8 Word8
  | TransformerPerspective -- Not fully supported, I think?
  | TransformerStroke Float Word8 Word8 Word8
    deriving (Eq, Show)

-- Decoding code

getSeveral :: Get a -> Get (Seq a)
getSeveral getter = do
  count <- getWord8
  S.fromList `fmap` replicateM (fromIntegral count) getter

pFile :: Get HVIFFile
pFile = do
  header <- getByteString 4
  when (header /= "ncif") $
    fail "Missing `ncif' header"
  hvifColors <- getSeveral pStyle
  hvifPaths  <- getSeveral pPath
  hvifShapes <- getSeveral pShape
  return HVIFFile { .. }

-- Style section

pStyle :: Get Style
pStyle = do
  sType <- getWord8
  case sType of
    0x01 -> ColorSolid <$> get <*> get <*> get <*> get
    0x02 -> ColorGradient <$> pGradient
    0x03 -> ColorSolidNoAlpha <$> get <*> get <*> get
    0x04 -> ColorSolidGray <$> get <*> get
    0x05 -> ColorSolidGrayNoAlpha <$> get
    _    -> getWord16be >> fail "invalid"

pGradient :: Get Gradient
pGradient = do
  gType  <- pGradientType
  gFlags <- pGradientFlags
  gStops <- getSeveral (pGradientStop gFlags)
  return Gradient { .. }

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

pGradientFlags :: Get GradientFlags
pGradientFlags = do
  gFlags <- getWord8
  return $ GradientFlags
    { gfTransform = testBit gFlags 1
    , gfNoAlpha   = testBit gFlags 2
    , gf16Bit     = testBit gFlags 3
    , gfGrays     = testBit gFlags 4
    }

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

pPath :: Get Path
pPath = do
  pathFlags <- pPathFlags
  pathPoints <- pPoints pathFlags
  return Path { .. }

pPathFlags :: Get PathFlags
pPathFlags = do
  pFlags <- getWord8
  return $ PathFlags
    { pfClosed       = testBit pFlags 1
    , pfUsesCommands = testBit pFlags 2
    , pfNoCurves     = testBit pFlags 3
    }

pPoints :: PathFlags -> Get (Seq Command)
pPoints PathFlags { pfUsesCommands = False
                  , pfNoCurves = False } =
  getSeveral pCurveCommand
pPoints PathFlags { pfUsesCommands = False
                  , pfNoCurves = True } =
  getSeveral pLineCommand
pPoints PathFlags { pfUsesCommands = True } =
  pCommandList

pLineCommand :: Get Command
pLineCommand = CmdLine <$> (Point <$> pCoord <*> pCoord)

pCurveCommand :: Get Command
pCurveCommand = CmdCurve <$> (Point <$> pCoord <*> pCoord)
                         <*> (Point <$> pCoord <*> pCoord)
                         <*> (Point <$> pCoord <*> pCoord)

pCommandList :: Get (Seq Command)
pCommandList = do
  pointCount <- getWord8
  let cmdByteCount = (pointCount + 3) `div` 4
  cmdBytes <- replicateM (fromIntegral cmdByteCount) getWord8
  let go n
        | n == fromIntegral pointCount = return S.empty
        | otherwise =
            let bIdx = n `div` 4
                iIdx = (n `mod` 4) * 2
            in case (cmdBytes !! bIdx) `shift` (negate iIdx) .&. 0x03 of
                 0x00 ->
                   (<|) <$> (CmdHLine <$> pCoord) <*> go (n+1)
                 0x01 ->
                   (<|) <$> (CmdVLine <$> pCoord) <*> go (n+1)
                 0x02 ->
                   (<|) <$> pLineCommand <*> go (n+1)
                 0x03 ->
                   (<|) <$> pCurveCommand <*> go (n+1)
                 _ -> error "[unreachable]"
  go 0

pCoord :: Get Float
pCoord = do
  b1 <- getWord8
  if testBit b1 7 then do
    b2 <- getWord8
    let cVal :: Word16 = (clearBit (fromIntegral b1) 7 `shift` 8) .|. fromIntegral b2
    return (fromIntegral cVal / 102.0 - 128.0)
  else
    return (fromIntegral b1 - 32.0)

ifFlag :: Bool -> Get a -> Get (Maybe a)
ifFlag True  m = Just <$> m
ifFlag False _ = pure Nothing

pShape :: Get Shape
pShape = do
  sType <- getWord8
  when (sType /= 0x0a) $
    fail ("Unknown shape type: " ++ show sType)
  shapeStyle <- StyleRef . fromIntegral <$> getWord8
  shapePaths <- getSeveral ((PathRef . fromIntegral) <$> getWord8)
  shapeFlags <- pShapeFlags
  shapeTransform <- ifFlag (sfTransform shapeFlags) $
    pMatrix
  shapeTranslate <- ifFlag (sfTranslation shapeFlags) $
    Translation <$> pCoord <*> pCoord
  shapeLodScale <- ifFlag (sfLodScale shapeFlags) $
    pLodScale
  shapeTransList <- if (sfHasTransformers shapeFlags)
    then getSeveral pTransformer
    else return S.empty
  return Shape { .. }

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

pMatrix :: Get Matrix
pMatrix = S.fromList `fmap` replicateM 6 pFloat

pFloat :: Get Float
pFloat = do
  b1 <- fromIntegral <$> getWord8
  b2 <- fromIntegral <$> getWord8
  b3 <- fromIntegral <$> getWord8
  let sVal :: Word32 = (b1 `shift` 16) .|. (b2 `shift` 8) .|. b3
      sMask = 0b100000000000000000000000 -- == 0x800000
      eMask = 0b011111100000000000000000 -- == 0x7e0000
      mMask = 0b000000011111111111111111 -- == 0x01ffff
      sign =  (sVal .&. sMask) `shift` (-23)
      expo = ((sVal .&. eMask) `shift` (-17)) - 32
      mant =  (sVal .&. mMask) `shift` 6
      val  = (sign `shift` 31) .|. ((expo + 127) `shift` 23) .|. mant
  if sVal == 0
    then return 0.0
    else castToFloat val

castToFloat :: Word32 -> Get Float
castToFloat w32 =
  let bs = encode w32
  in case runGet getFloat32be bs of
    Left err -> fail err
    Right x  -> return x

pLodScale :: Get LodScale
pLodScale = do
  minS <- fromIntegral <$> getWord8
  maxS <- fromIntegral <$> getWord8
  return LodScale
    { lsMin = minS / 63.75
    , lsMax = maxS / 63.75
    }

pTransformer :: Get Transformer
pTransformer = do
  tType <- getWord8
  case tType of
    20 -> TransformerAffine <$> pMatrix
    21 -> do
      width      <- fromIntegral <$> getWord8
      lineJoin   <- getWord8
      miterLimit <- getWord8
      return (TransformerContour (width - 128.0) lineJoin miterLimit)
    22 -> pure TransformerPerspective
    23 -> do
      width <- fromIntegral <$> getWord8
      lineOptions <- getWord8
      miterLimit  <- getWord8
      let lineJoin = lineOptions .&. 15
          lineCap  = lineOptions `shift` 4
      return (TransformerStroke (width - 128.0) lineJoin lineCap miterLimit)
    _ -> fail ("Unknown transformer type: " ++ show tType)
