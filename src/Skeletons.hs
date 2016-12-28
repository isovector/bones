{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skeletons where

import Data.AffineSpace
import Data.VectorSpace
import Math.MFSolve
import Data.Map (Map)

type V3 a = (a, a, a)

mkV3 a b c = (a, b, c)

data AbsRel a = Abs a | Rel a

type Movement n a = Map Double [(n, a)]
type Assignment n a = n -> a

solve :: Skeleton' n a
      -> Assignment n a
      -> Movement n a
      -> Double
      -> Assignment n a
-- sort the movement by timestamp, at each one, solve the constraint
-- system and then lerp between them for the output
solve = undefined

data Skeleton' n a = Joint
  { _jointName :: n
  , _jointPos  :: a
  , _jointSubs :: [Skeleton' n a]
  } deriving (Eq, Show)

data Bone' n a = Bone
  { _boneJoint1 :: n
  , _boneJoint2 :: n
  , _boneLength :: a
  } deriving (Eq, Show)

computeBones :: forall n a.
                ( AffineSpace a
                , Floating (Scalar (Diff a))
                , InnerSpace (Diff a)
                )
             => Skeleton' n a
             -> [Bone' n (Scalar (Diff a))]
computeBones = go <*> _jointSubs
  where
    go parent [] = []
    go parent children = do
      child <- children
      let overBoth :: (Skeleton' n a -> b) -> (b, b)
          overBoth f = (f parent, f child)
      let bone = uncurry Bone (overBoth _jointName)
                  $ uncurry distance $ overBoth _jointPos
      bone : go child (_jointSubs child)


skeleton :: Skeleton' String (Double, Double, Double)
skeleton = Joint "Torso" zeroV
         [ Joint "Head" (mkV3 0 1 0) []
         , Joint "Pelvis" (mkV3 0 (-3) 0)
           [ Joint "LeftLeg" (mkV3 (-1) (-5) 0) []
           , Joint "RightLeg" (mkV3 (1) (-5) 0) []
           ]
         ]
