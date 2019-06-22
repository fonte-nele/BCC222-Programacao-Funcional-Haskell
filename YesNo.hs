module YesNo where

class YesNo t where
  yesno :: t -> Bool

instance YesNo Bool where
  yesno x = x

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
data Semaforo = Verde | Amarelo | Vermelho
              deriving (Show)

instance YesNo Semaforo where
  yesno Verde = True
  yesno _ = False

yesnoIf :: (YesNo a) => a -> b -> b -> b
yesnoIf test alt1 alt2 | yesno test = alt1
                       | otherwise = alt2
