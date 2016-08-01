{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Data.Text.IO as TIO
  import NLP.PStemmer.Ru

  main :: IO ()
  main = TIO.putStrLn $ runPorter "бегавшая"
