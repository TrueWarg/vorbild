module Vorbild.Either where

eitherZipWith :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
eitherZipWith f (Right a) (Right b) = Right $ f a b
eitherZipWith _ (Left e) _          = Left e
eitherZipWith _ _ (Left e)          = Left e

isRight :: Either e a -> Bool
isRight mea =
  case mea of
    Right _ -> True
    _       -> False

eitherZipWith3 ::
     (a -> b -> c -> d)
  -> Either e a
  -> Either e b
  -> Either e c
  -> Either e d
eitherZipWith3 f ea eb ec =
  let abRes = eitherZipWith (\a b -> (a, b)) ea eb
   in eitherZipWith (\(a, b) c -> f a b c) abRes ec

eitherZipWith4 ::
     (a -> b -> c -> d -> f)
  -> Either e a
  -> Either e b
  -> Either e c
  -> Either e d
  -> Either e f
eitherZipWith4 f ea eb ec ed =
  let abRes = eitherZipWith (\a b -> (a, b)) ea eb
      cdRes = eitherZipWith (\c d -> (c, d)) ec ed
   in eitherZipWith (\(a, b) (c, d) -> f a b c d) abRes cdRes

accumulate :: Semigroup a => Either e a -> Either e a -> Either e a
accumulate (Right item1) (Right item2) = Right (item1 <> item2)
accumulate (Left item) _             = Left item
accumulate _ (Left item)            = Left item

accumulateWithList :: Either e [a] -> Either e a -> Either e [a]
accumulateWithList (Right acc) (Right item) = Right (item : acc)
accumulateWithList (Left acc) _             = Left acc
accumulateWithList _ (Left item)            = Left item

rightAccumulateWithList :: Either e a -> Either e [a] -> Either e [a]
rightAccumulateWithList (Right item) (Right acc) = Right (item : acc)
rightAccumulateWithList (Left acc) _             = Left acc
rightAccumulateWithList _ (Left item)            = Left item
