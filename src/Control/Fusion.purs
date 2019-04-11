module Control.Fusion
       ( module FusionTypes
       , class Fusion, runFusion
       )
where

import Prelude

import Control.Fusion.Types (Identity(..), Length(..), Reverse(..), Foldr(..), Foldl(..), Map(..), Compose(..), (:<<<))
import Control.Fusion.Types as FusionTypes
import Data.Array as A
import Data.Foldable (foldl, foldr)

-- | Class for data types that represent functions that can be fused.
class Fusion f a b | f a -> b where
  runFusion :: f -> a -> b

instance functionFusion :: Fusion (a -> b) a b where
  runFusion f = f

-- *** Instances for datatype representations of normal functions.

instance fusionIdentity :: Fusion Identity a a where
  runFusion Identity = identity

instance fusionReverse :: Fusion Reverse (Array a) (Array a) where
  runFusion Reverse = A.reverse

instance fusionLength :: Fusion Length (Array a) Int where
  runFusion Length = A.length

instance fusionFoldr :: (Fusion f a (b -> b)) => Fusion (Foldr f b) (Array a) b where
  runFusion (Foldr f z) = foldr (runFusion f) z

instance fusionFoldl :: (Fusion f b (a -> b)) => Fusion (Foldl f b) (Array a) b where
  runFusion (Foldl f z) = foldl (runFusion f) z

instance fusionMapArrays :: (Fusion f a b, Functor t) => Fusion (Map f) (t a) (t b) where
  runFusion (Map f) = map (runFusion f)


-- *** Fusion laws should go here -- above most general instances and below
-- instances for normal functions.
-- Instance chains are required.

-- * Law: identity <<< identity = identity

else instance lawComposeIdentityIdentity
  :: Fusion (Compose Identity Identity) a a where
  runFusion _ = identity

-- * Law: map identity = identity

-- map identity = identity
else instance lawMapIdentity
  :: Fusion (Map Identity) a a where
  runFusion _ = identity


-- * Law: map f <<< map g = map (f <<< g)

-- map h <<< map g <<< f -> map (h <<< g) <<< f
else instance lawComposeMapComposeMap
  :: ( Fusion f (t a) (t b)
     , Fusion g b c
     , Fusion h c d
     , Functor t
     )
  => Fusion (Compose (Map h) (Compose (Map g) f)) (t a) (t d) where
  runFusion (Map h :<<< (Map g :<<< f)) =
    let g' = runFusion g :: b -> c
        h' = runFusion h :: c -> d in
    runFusion $ Map (h' <<< g') :<<< f

-- map f <<< map g -> map (f <<< g)
else instance lawComposeMapMap
  :: ( Fusion f a b
     , Fusion g b c
     , Functor t
     )
  => Fusion (Compose (Map f) (Map g)) (t a) (t c) where
  runFusion (Map g :<<< Map f) =
    let f' = runFusion f :: b -> c
        g' = runFusion g :: a -> b in
      runFusion $ Map (f' <<< g')


-- * Law: foldr f c <<< map g = foldr (f <<< g) c

-- foldr f c <<< map g = foldr (f <<< g) c
else instance lawComposeFoldrMap
  :: ( Fusion g a b
     , Fusion f b (c -> c)
     )
  => Fusion (Compose (Foldr f c) (Map g)) (Array a) c where
  runFusion (Foldr f c :<<< Map g) =
    let g' = runFusion g :: a -> b
        f' = runFusion f :: b -> c -> c in
      runFusion $ Foldr (f' <<< g') c


-- *** Most general cases

-- We just don't want left associativity. Otherwise there would be more cases to
-- consider for each law.
else instance ditchLeftAssocCompose
  :: ( Fusion f a b
     , Fusion g b c
     , Fusion h c d
     )
   => Fusion (Compose (Compose h g) f) a d where
  runFusion ((h :<<< g) :<<< f) = runFusion $ h :<<< (g :<<< f)

else instance fusionCompose
  :: ( Fusion f a b
     , Fusion g b c
     )
  => Fusion (Compose g f) a c where
  runFusion (f :<<< g) = runFusion f <<< runFusion g
