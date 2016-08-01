module NLP.PStemmer.Internal.Function (
  module PST,
  module NLP.PStemmer.Internal.Function,
  fromMaybe,
  first, second, (***), (&&&)) where

  import qualified Data.Text as T
  import qualified Data.List as DL
  import qualified Data.Ord as DO
  import Data.Maybe (fromMaybe)
  import NLP.PStemmer.Types as PST
  import Control.Arrow (second, first, (***), (&&&))

  applySteps :: T.Text -> [Step] -> T.Text
  applySteps rv [] = rv
  applySteps rv (f:fs) =
    let
      r :: T.Text
      r = f rv
    in
      if T.null r
        then r
        else applySteps r fs

  cutList :: List -> Substep
  cutList (w:ws) txt =
    if w `T.isSuffixOf` txt
      then fromMaybe (txt, False) (flip (,) True <$> T.stripSuffix w txt)
      else cutList ws txt
  cutList _ txt = (txt, False)

  getFirstSR :: [Substep] -> T.Text -> T.Text
  getFirstSR [] txt = txt
  getFirstSR (f:fs) txt =
    let
      r :: StepResult
      r = f txt
    in
      if snd r
        then fst r
        else getFirstSR fs txt

  sortByLengthDown :: List -> List
  sortByLengthDown = DL.sortBy (DO.comparing (DO.Down . T.length))
