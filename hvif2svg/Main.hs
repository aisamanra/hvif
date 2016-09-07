{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import           Control.Monad (forM_)
import qualified Data.ByteString as BS
import           Data.Foldable (toList)
import           Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as S
import           Graphics.HVIF

main :: IO ()
main = do
  contents <- BS.getContents
  case decodeFile contents of
    Left err -> putStrLn err
    Right hvif -> do
      putStrLn "<svg width=\"200\" height=\"200\" xmlns=\"http://www.w3.org/2000/svg\">"
      putStrLn "<defs>"
      forM_ [ (i, c) | i <- [0..]
                     | c <- toList (hvifColors hvif) ] $ \ (idx, style) ->
        case style of
          ColorGradient g -> printGradient idx g
          _ -> return ()
      putStrLn "</defs>"
      forM_ (hvifShapes hvif) $ \shape -> do
        putStr "<path d=\""
        let paths = [ hvifPaths hvif `S.index` p
                    | PathRef p <- toList (shapePaths shape)
                    ]
        let tr = case shapeTransform shape of
              Just m  -> transform m
              Nothing -> id
        mapM_ (drawPoints True 0.0 0.0 tr . pathPoints) paths
        putStr "fill=\""
        let styleId = stIdx (shapeStyle shape)
        drawStyle styleId (hvifColors hvif `S.index` stIdx (shapeStyle shape))
        putStrLn "\"/>"
      putStrLn "</svg>"

percent :: Integral a => a -> String
percent n = show (floor ((fromIntegral n / 255.0) * 100.0 :: Float))

printGradient :: Int -> Gradient -> IO ()
printGradient n g = do
  let gname = "grad" ++ show n
  putStrLn ("<linearGradient id=\"" ++ gname ++
            "\" x1=\"100%\" y1=\"0%\" x2=\"0%\" y2=\"100%\">")
  forM_ (gStops g) $ \ (GradientStop off r g b a) -> do
    putStrLn $ concat [ "<stop offset=\""
                      , percent off
                      , "%\" stop-color=\"rgba("
                      , show r
                      , ","
                      , show g
                      , ","
                      , show b
                      , ","
                      , show (fromIntegral a / 255.0)
                      , ")\"/>"
                      ]
  putStrLn "</linearGradient>"

type Transformer = (Float, Float) -> (Float, Float)

drawPoints :: Bool -> Float -> Float -> Transformer -> Seq Command -> IO ()
drawPoints first lx ly tr seq
  | cmd :< xs <- S.viewl seq = do
      let dir = if first then "M" else " L"
          showPt x y = do
            let (x', y') = tr (x, y)
            putStr $ unwords [dir, show (floor x'), show (floor y')]
      case cmd of
        CmdLine (Point x y) -> do
          showPt x y
          drawPoints False x y tr xs
        CmdHLine x -> do
          showPt x ly
          drawPoints False x ly tr xs
        CmdVLine y -> do
          showPt lx y
          drawPoints False lx y tr xs
        CmdCurve (Point x y) _ _ -> do
          showPt x y
          drawPoints False x y tr xs
  | otherwise = putStr " Z\" "

transform :: Seq Float -> (Float, Float) -> (Float, Float)
transform (toList-> [m0,m1,m2,m3,m4,m5]) (x, y) =
  (x * m0 + y * m2 + m4, x * m1 + y * m3 + m5)

drawStyle :: Int -> Style -> IO ()
drawStyle _ (ColorSolid r g b a) = putStr $ concat
  ["rgba(", show r, ",", show g, ",", show b, ",", show (fromIntegral a / 255.0), ")"]
drawStyle _ (ColorSolidNoAlpha r g b) = putStr $ concat
  ["rgb(", show r, ",", show g, ",", show b, ")"]
drawStyle _ (ColorSolidGray g a) = putStr $ concat
  ["rgba(", show g, ",", show g, ",", show g, ",", show (fromIntegral a / 255.0), ")"]
drawStyle _ (ColorSolidGrayNoAlpha g) = putStr $ concat
  ["rgb(", show g, ",", show g, ",", show g, ")"]
drawStyle n _ = putStr ("url(#grad" ++ show n ++ ")")
