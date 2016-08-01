{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Data.Text.IO as TIO
  import PStemmer.Ru

  main :: IO ()
  main = TIO.putStrLn $ runPorter "бегавшая"
