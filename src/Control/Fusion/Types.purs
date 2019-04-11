module Control.Fusion.Types where

data Identity = Identity
data Length = Length
data Reverse = Reverse
data Foldr f a = Foldr f a
data Foldl f a = Foldl f a
newtype Map f = Map f

data Compose f g = Compose f g
infixr 9 Compose as :<<<

identity :: Identity
identity = Identity

length :: Length
length = Length

foldr :: forall f z. f -> z -> Foldr f z
foldr = Foldr

foldl :: forall f z. f -> z -> Foldl f z
foldl = Foldl

map :: forall a. a -> Map a
map = Map
