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

accumulate :: Semigroup a => Either e a -> Either e a -> Either e a
accumulate (Right acc) (Right item) = Right (acc <> item)
accumulate (Left acc) _             = Left acc
accumulate _ (Left item)            = Left item

accumulateWithList :: Either e [a] -> Either e a -> Either e [a]
accumulateWithList (Right acc) (Right item) = Right (item : acc)
accumulateWithList (Left acc) _             = Left acc
accumulateWithList _ (Left item)            = Left item
