# pstemmer
A Haskell Implementation of the [Porter Stemmer](https://tartarus.org/martin/index.html) for [Russian](http://snowball.tartarus.org/algorithms/russian/stemmer.html)

# Usage
```haskell
import Data.Text.IO as TIO
import NLP.PStemmer.Ru

main :: IO ()
main = TIO.putStrLn $ runPorter "бегавшая"
```
