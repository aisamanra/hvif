{-# LANGUAGE PatternGuards #-}

module Main where

import           Control.Monad (forM_)
import qualified Data.ByteString as BS
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
      forM_ (hvifPaths hvif) $ \path -> do
        putStr "<path d=\""
        drawPoints True 0.0 0.0 (pathPoints path)
        putStrLn "stroke=\"black\"/>"
      putStrLn "</svg>"

drawPoints :: Bool -> Float -> Float -> Seq Command -> IO ()
drawPoints first lx ly seq
  | cmd :< xs <- S.viewl seq = do
      let dir = if first then "M" else "L"
      case cmd of
        CmdLine (Point x y) -> do
          putStr $ unwords [dir, show (floor x), show (floor y)]
          drawPoints False x y xs
        CmdHLine x -> do
          putStr $ unwords [dir, show (floor x), show (floor ly)]
          drawPoints False x ly xs
        CmdVLine y -> do
          putStr $ unwords [dir, show (floor lx), show (floor y)]
          drawPoints False lx y xs
        CmdCurve (Point x y) _ _ -> do
          putStr $ unwords [dir, show (floor x), show (floor y)]
          drawPoints False x y xs
  | otherwise = putStr "Z\" "
