module PTableM where

  type Events = [String]
  type Probs = [Double]

  data PTable = PTable Events Probs

  createPTable :: Events -> Probs -> PTable
  createPTable ev pr = 
    PTable ev $ normalizedPr
    where 
      normalizedPr = map (\x -> x / totalProbs) pr
      totalProbs = sum pr

  showPair :: String -> Double -> String
  showPair ev pr = 
    mconcat [ev, "|", show pr, "\n"]

  instance Show PTable where
    show (PTable ev pr) = 
      mconcat pairs
      where
        pairs = zipWith showPair ev pr

  cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
  cartCombine f list1 list2 = 
    zipWith f newL1 cycledL2
      where
        nToAdd = length list2
        repeatedL1 = map (take nToAdd . repeat) list1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle list2

  combineEvents :: Events -> Events -> Events
  combineEvents = cartCombine combiner 
    where
      combiner = (\x y -> mconcat [x, "-", y])
  
  combineProbs :: Probs -> Probs -> Probs
  combineProbs = cartCombine (*)

  instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = 
      createPTable newEvents newProbs
        where
          newEvents = combineEvents e1 e2
          newProbs = combineProbs p1 p2
  
  instance Monoid PTable where
    mempty = PTable [] []

  coin = PTable ["heads", "tails"] [0.5, 0.5]
  spinner = PTable ["red", "blue", "green"] [0.1, 0.2, 0.7]