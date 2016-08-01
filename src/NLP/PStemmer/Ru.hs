{-# LANGUAGE OverloadedStrings #-}

module NLP.PStemmer.Ru where

  import qualified Data.Text as T

  import NLP.PStemmer.Internal.Function

  type ListG = [(T.Text, [T.Text])]

  runPorter' :: [T.Text] -> [(T.Text, T.Text)]
  runPorter' = map (id &&& runPorter)

  runPorter :: T.Text -> T.Text
  runPorter txt =
    let
      rv :: RV
      rv = mkRV txt
    in
      fst rv `T.append` applySteps (snd rv) [step1, step2, step3, step4]

  step4 :: Step
  step4 txt
    | T.null txt = txt
    | otherwise = getFirstSR [step4_1, step4_2, step4_3] txt
    where
      step4_1 :: Substep
      step4_1 txt' =
        if "нн" `T.isSuffixOf` txt'
          then (T.init txt', True)
          else (txt', False)

      step4_2 :: Substep
      step4_2 txt' =
        case cutList mapSuperl txt' of
          (t, True) -> (fst $ step4_1 t, True)
          _ -> (txt', False)

      step4_3 :: Substep
      step4_3 txt' =
        if T.last txt' == 'ь'
          then (T.init txt', True)
          else (txt', False)

  step3 :: Step
  step3 txt
    | T.null txt = txt
    | otherwise =
      let
        r2 :: Maybe T.Text
        r2 = snd . mkR1R2 $ txt
      in
        case r2 of
          Just t ->
            if any (`T.isSuffixOf` t) mapDerivat
              then fst . cutList mapDerivat $ txt
              else txt
          _ -> txt

  step2 :: Step
  step2 txt
    | T.null txt = txt
    | otherwise =
      if T.last txt == 'и'
        then T.init txt
        else txt

  step1 :: Step
  step1 txt
    | T.null txt = txt
    | otherwise = getFirstSR [step1_1, step1_2, step1_3, step1_4, step1_5, step1_6, step1_7, step1_8, step1_9] txt
    where
      step1_1 :: Substep
      step1_1 = cutList mapPG_G2

      step1_2 :: Substep
      step1_2 = cutListG mapPG_G1

      step1_3 :: Substep
      step1_3 t = second (const False) $ cutList mapRefl t

      step1_4 :: Substep
      step1_4 = cutList mapPartic_G2

      step1_5 :: Substep
      step1_5 = cutListG mapPartic_G1

      step1_6 :: Substep
      step1_6 = cutList mapADJ

      step1_7 :: Substep
      step1_7 = cutList mapVerb_G2

      step1_8 :: Substep
      step1_8 = cutListG mapVerb_G1

      step1_9 :: Substep
      step1_9 = cutList mapNoun

  cutListG :: ListG -> Substep
  cutListG ((w, ws):ls) txt =
    if any (`T.isSuffixOf` txt) ws
      then fromMaybe (txt, False) (flip (,) True <$> T.stripSuffix w txt)
      else cutListG ls txt
  cutListG _ txt = (txt, False)

  prepareList :: List -> List
  prepareList = sortByLengthDown

  prepareListWith :: List -> List -> List
  prepareListWith sl al = sortByLengthDown . concatMap (\w -> map (w `T.append`) al) $ sl

  prepareListG :: List -> ListG
  prepareListG = map (\w -> (w, map (`T.cons` w) neccSuff)) . sortByLengthDown

  prepareListGWith :: List -> List -> ListG
  prepareListGWith sl al = map (\w -> (w, map (`T.cons` w) neccSuff)) .
    sortByLengthDown . concatMap (\w -> map (w `T.append`) al) $ sl

  mkRV :: T.Text -> RV
  mkRV txt =
    let
      (p1,p2) = T.break (`elem` vowels) txt
    in
      case T.uncons p2 of
        Just (ch, p2') -> (p1 `T.snoc` ch, p2')
        _ -> (p1,p2)

  mkR1R2 :: T.Text -> R1R2
  mkR1R2 txt
    | T.null txt = r1r2null
    | otherwise = mkR' (T.tails txt) r1r2null
    where
      mkR' :: [T.Text] -> R1R2 -> R1R2
      mkR' _ r1r2@(Just _, Just _) = r1r2
      mkR' (t1:t2:xs) r1r2@(Nothing, r2@Nothing)
        | T.head t1 `notElem` vowels = mkR' (t2:xs) (tNull t2, r2)
        | otherwise = mkR' (t2:xs) r1r2
      mkR' (t1:t2:t3:xs) r1r2@(r1@(Just _), Nothing)
        | chkV t1 t2 = mkR' (t3:xs) (r1, tNull t3)
        | otherwise = mkR' (t2:t3:xs) r1r2
      mkR' _ r1r2 = r1r2

      r1r2null :: R1R2
      r1r2null = (Nothing, Nothing)

      tNull :: T.Text -> Maybe T.Text
      tNull t
        | T.null t = Nothing
        | otherwise = Just t

      chkV :: T.Text -> T.Text -> Bool
      chkV t1 t2 = (T.head t1 `elem` vowels) && (T.head t2 `notElem` vowels)

  -- -----------------------------------------------------------------------------
  -- * Data of Poter stemmer for Russian language

  neccSuff :: String
  neccSuff = "ая"

  mapPG_G1 :: ListG
  mapPG_G1 = prepareListG ["в", "вши", "вшись"]

  mapPG_G2 :: List
  mapPG_G2 = prepareList ["ив", "ивши", "ившись", "ыв", "ывши", "ывшись"]

  mapADJ :: List
  mapADJ = prepareList ["ее", "её", "ёе", "ёё", "ие", "иё", "ые", "ыё", "ое", "оё",
    "ими", "ыми", "ей", "ёй", "ий", "ый", "ой", "ем", "ём", "им", "ым", "ом", "его",
    "ёго", "ого", "ему", "ёму", "ому", "их", "ых", "ую", "юю", "ая", "яя", "ою",
    "ею", "ёю"]

  mapPartic_G1 :: ListG
  mapPartic_G1 = prepareListGWith ["ем", "ём", "нн", "вш", "ющ", "щ"] mapADJ

  mapPartic_G2 :: List
  mapPartic_G2 = prepareListWith ["ивш", "ывш", "ующ"] mapADJ

  mapRefl :: List
  mapRefl = prepareList ["ся", "сь"]

  mapVerb_G1 :: ListG
  mapVerb_G1 = prepareListG ["ла", "на", "ете", "ёте", "етё", "ётё", "йте", "йтё", "ли",
    "й", "л", "ем", "ём", "н", "ло", "но", "ет", "ёт", "ют", "ны", "ть", "ешь", "ёшь",
    "нно"]

  mapVerb_G2 :: List
  mapVerb_G2 = prepareList ["ила", "ыла", "ена", "ёна", "ейте", "ёйте", "ейтё", "ёйтё",
    "уйте", "уйтё", "ите", "итё", "или", "ыли", "ей", "ёй", "уй", "ил", "ыл", "им", "ым",
    "ен", "ён", "ило", "ыло", "ено", "ёно", "ят", "ует", "уёт", "уют", "ит", "ыт", "ены",
    "ёны", "ить", "ыть", "ишь", "ую", "ю"]

  mapNoun :: List
  mapNoun = prepareList ["а", "ев", "ёв", "ов", "ие", "иё", "ье", "е", "ё", "иями",
    "ями", "ами", "еи", "ёи", "ии", "и", "ией", "иёй", "ей", "ёй", "ой", "ий", "й",
    "иям", "ям", "ием", "иём", "ем", "ём", "ам", "ом", "о", "у", "ах", "иях", "ях",
    "ы", "ь", "ию", "ью", "ю", "ия", "ья", "я"]

  mapSuperl :: List
  mapSuperl = prepareList ["ейш", "ёйш", "ейше", "ёйше", "ейшё", "ёйшё"]

  mapDerivat :: List
  mapDerivat = prepareList ["ост", "ость"]

  vowels :: String
  vowels = "аеёиоуыэюя"
