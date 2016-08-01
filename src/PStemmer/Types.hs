module PStemmer.Types where

  import qualified Data.Text as T

  type R1R2 = (Maybe T.Text, Maybe T.Text)

  type List = [T.Text]

  type RV = (T.Text, T.Text)

  type StepResult = (T.Text, Bool)

  type Step = T.Text -> T.Text

  type Substep = T.Text -> StepResult
